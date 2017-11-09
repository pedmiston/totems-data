import tasks
from tasks.paths import R_PKG, Path


@task
def use_data(ctx, use_data_script=None, clear_data_dir=False):
    """Compile data to .rda in totems R pkg."""
    if use_data_script is None:
        use_data_scripts = Path(R_PKG, 'data-raw/').listdir('use-data*.R')
    else:
        use_data_scripts = [Path(R_PKG, 'data-raw/', use_data_script + '.R')]

    if clear_data_dir:
        ctx.run('cd {R_pkg} && rm -rf data/*.rda'.format(R_pkg=R_PKG), echo=True)

    cmd = 'cd {R_pkg} && Rscript {use_data_script}'
    for use_data_script in use_data_scripts:
        ctx.run(cmd.format(R_pkg=R_PKG, use_data_script=use_data_script),
                echo=True)


@task
def install(ctx, use_data_too=False, make_graph=False,
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
        ctx.run(cmd.format(R_pkg=R_PKG, R_cmds=crotchet_prereq))

    if use_data_too or use_data_script or clear_data_dir:
        use_data(ctx, use_data_script=use_data_script,
                 clear_data_dir=clear_data_dir)

    if make_graph:
        tasks.graph.tree(ctx, view_off=True)
        tasks.graph.tree(ctx, max_generation=6, name='landscape-sample',
                         view_off=True)
        tasks.graph.tree(ctx, max_number=100, name='landscape-tools',
                         view_off=True)

    if document_only:
        R_cmds = ["devtools::document()"]

    ctx.run(cmd.format(R_pkg=R_PKG, R_cmds=';'.join(R_cmds)))
