from pathlib import Path
from invoke import task
import graphdb
import pandas


R_pkg = Path(__file__).absolute().parent
ITEM_IMAGES = Path(R_pkg, 'inst/extdata/items')


@task
def install(ctx, use_data_too=False, make_trees=False,
            use_data_script=None, document_only=False,
            clear_data_dir=False, ignore_prereqs=False):
    """Install the totems R pkg."""
    cmd = 'cd {R_pkg} && Rscript -e "{R_cmds}"'
    R_cmds = """
    library(devtools)
    document()
    install()
    """.split()

    if not ignore_prereqs:
        crotchet_prereq = "devtools::install_github('pedmiston/crotchet')"
        ctx.run(cmd.format(R_pkg=R_pkg, R_cmds=crotchet_prereq))

    if use_data_too or use_data_script or clear_data_dir:
        use_data(ctx, clear_data_dir=clear_data_dir)

    if make_trees:
        tree(ctx, view_off=True)
        tree(ctx, max_generation=6, name='landscape-sample',
             view_off=True)
        tree(ctx, max_number=100, name='landscape-tools',
             view_off=True)

    if document_only:
        R_cmds = ["devtools::document()"]

    ctx.run(cmd.format(R_pkg=R_pkg, R_cmds=';'.join(R_cmds)))

@task
def get_raw_data(ctx):
    """Download the raw data as csvs in the R pkg."""
    ctx.run(f'cd {R_pkg} && Rscript make-data-raw.R', echo=True)

@task
def use_data(ctx, clear_data_dir=False):
    """Compile data to .rda in totems R pkg."""
    if clear_data_dir:
        ctx.run('cd {R_pkg} && rm -rf data/*.rda'.format(R_pkg=R_pkg), echo=True)

    cmd = 'cd {R_pkg} && Rscript make-data.R'
    ctx.run(cmd.format(R_pkg=R_pkg), echo=True)

@task
def tree(ctx, max_number=None, max_generation=None, name=None, view_off=False):
    """Visualize the totems landscape in a figure.

    Examples:

        inv tree
        inv tree -n landscape-sample --max-generation 6
        inv tree -n landscape-tools --max-number 100
    """
    viz = graphdb.make_landscape(image_dir=ITEM_IMAGES,
                                 max_number=max_number,
                                 max_generation=max_generation)
    viz.format = 'png'
    name = name or 'landscape'
    output = Path(R_pkg, 'inst/extdata/', name+'.gv')
    viz.render(output, view=not view_off)

@task
def adjacent(ctx, inventory_ids_csv='data-raw/adjacent/inventory-ids.csv'):
    """Analyze the items adjacent to a list of inventory ids."""
    analyze_inventory_ids(inventory_ids_csv)

@task
def inventory(ctx, item_numbers=None, name=None, view_off=False):
    """Visualize an inventory with a figure."""
    if item_numbers is None:
        item_numbers = [1, 2, 3, 4, 5, 6,
                        11, 12, 13, 14, 15, 16, 17,
                        23, 24]
    else:
        item_numbers = list(map(int, item_numbers.split(',')))
    viz = graphdb.make_inventory(image_dir=ITEM_IMAGES, item_numbers=item_numbers)
    viz.format = 'png'
    name = name or 'inventory'
    output = Path(R_pkg, 'inst/extdata/', name+'.gv')
    viz.render(output, view=not view_off)

def analyze_inventory_ids(inventory_ids_csv):
    inventory_ids_csv = Path(inventory_ids_csv)
    adjacent_dir = inventory_ids_csv.parent

    inventories = pandas.read_csv(inventory_ids_csv)

    def id_to_numbers(inventory_id):
        return list(map(int, inventory_id.split('-')))

    inventories['Inventory'] = inventories.ID.apply(id_to_numbers)

    landscape = graphdb.Landscape()
    inventories['Adjacent'] = inventories.Inventory.apply(landscape.adjacent_possible)

    inventories['NumAdjacent'] = inventories.Adjacent.apply(len)
    inventories[['ID', 'NumAdjacent']].to_csv(
      Path(adjacent_dir, 'num-adjacent.csv'),
      index=False
    )

    def melt_adjacent(inventory):
        return pandas.DataFrame({'Adjacent': inventory.Adjacent.iloc[0]})

    adjacent_inventories = (inventories.groupby('ID')
                                       .apply(melt_adjacent)
                                       .reset_index())

    adjacent_inventories[['ID', 'Adjacent']].to_csv(
      Path(adjacent_dir, 'adjacent-items.csv'),
      index=False
    )
