# totems

`totems` is an R package containing the data and helper functions
for analyzing the results of the Totems experiment.

## Downloading database tables

```R
devtools::load_all()
create_blank_config()
# edit "config.yml"
save_all_tables()  # save all tables to "data-raw/tables/"
```
