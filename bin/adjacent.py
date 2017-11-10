#!/usr/bin/env python
import sys
from pathlib import Path
import pandas
import graphdb

assert len(sys.argv) == 2 and Path(sys.argv[1]).exists()
inventory_ids_csv = Path(sys.argv[1])
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
