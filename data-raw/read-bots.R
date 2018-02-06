library(readr)
library(devtools)

read_bots_experiment <- function(stem) {
  filename <- paste0(stem, ".csv")
  csv <- file.path("data-raw/bots", filename)
  readr::read_csv(csv)
}

BotsStrategy <- read_bots_experiment("strategy")
BotsPlayers <- read_bots_experiment("players")
BotsMemory <- read_bots_experiment("memory")

use_data(BotsStrategy, BotsPlayers, BotsMemory, overwrite = TRUE)
