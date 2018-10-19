library(readr)
library(devtools)

read_bots_experiment <- function(stem) {
  filename <- paste0(stem, ".csv")
  csv <- file.path("data-raw/bots", filename)
  readr::read_csv(csv)
}

BotsPlayers <- read_bots_experiment("players")

use_data(BotsPlayers, overwrite = TRUE)
