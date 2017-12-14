#!/usr/bin/env Rscript
devtools::load_all()

data("Sessions")

exp1 <- Sessions %>%
  dplyr::filter(Strategy == "Diachronic")
write.csv(exp1, "data-raw/manifest-exp1.csv", row.names = FALSE)
