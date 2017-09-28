library(devtools)

# Load util functions from the package.
# WARNING! Don't load the package or use_data() won't work
library(magrittr)
library(tools)
library(readr)
source("R/util.R")

read_csvs("data-raw/simulations", prefix = "Bots",
          stem_func = stringr::str_to_title)

use_data(
  BotsExplore,
  BotsReplication,
  BotsStrategy,
  BotsPlayers,
  BotsMemory,
  BotsGuesses,
  overwrite = TRUE
)
