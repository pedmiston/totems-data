#!/usr/bin/env Rscript
unlink("data/", recursive = TRUE)
devtools::load_all()

source("data-raw/1-teams-and-sessions.R")
source("data-raw/2-guesses.R")
source("data-raw/item-labels.R")
