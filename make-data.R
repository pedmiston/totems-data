#!/usr/bin/env Rscript
unlink("data/", recursive = TRUE)
devtools::load_all()

source("bin/0-replace-strategy.R")
source("bin/1-teams-and-sessions.R")
source("bin/2-guesses.R")
source("bin/item-labels.R")
source("bin/read-manifests.R")
source("bin/read-bots.R")
source("bin/read-subj-info.R")
