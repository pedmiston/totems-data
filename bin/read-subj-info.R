library(devtools)
library(tidyverse)

SubjInfoSheets <- list.files("data-raw/subj-info", full.names = TRUE) %>%
  map(read_csv)
names(SubjInfoSheets) <- list.files("data-raw/subj-info") %>%
  str_split_fixed(".csv", 2) %>%
  .[,1]

use_data(SubjInfoSheets, overwrite = TRUE)
