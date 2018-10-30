# totems

`totems` is an R package containing the data and helper functions
for analyzing the results of experiments using the Totem game.

## Installing the data in an R package

You can install the "totems" package directly from GitHub using
the "remotes" package.

```R
remotes::install_github("pedmiston/totems-data")
```

## Downloading database tables

To download the database tables as csvs, you need to create
a "config.yml" file with the correct authentication information
for connecting to the database.

```R
devtools::load_all()
create_blank_config()
# manually edit "config.yml"
save_all_tables()  # saves tables to data-raw/tables/*.csv
```

## Downloading lab notes and post-experiment survey responses

```R
save_lab_notes()
save_survey_responses()
```

## Creating the datasets included in the R package

```R
source("make-data.R")
# creates data/*.rda
```

## Installing this package locally

```R
devtools::document()
devtools::install()
```
