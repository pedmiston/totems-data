unlink("data/", recursive = TRUE)
devtools::load_all()

save_all_tables()
save_subj_info()
save_survey_responses()

source("data-raw/1-teams-and-players.R")
source("data-raw/2-guesses.R")
source("data-raw/diachronic-performance.R")

source("data-raw/item-labels.R")
