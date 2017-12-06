# totems

`totems` is an R package containing the data and helper functions
for analyzing the results of the Totems experiment.

## Downloading database tables

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
