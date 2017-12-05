#' Save subject info from Google Drive spreadsheet.
#'
#' @import googlesheets
save_subj_info <- function() {
  dir.create("data-raw/subj-info", showWarnings = FALSE)
  subj_info <- gs_title("totems-subj-info")
  gs_download(subj_info, ws = "TOT/TOI", to = "data-raw/subj-info/tot-toi.csv", overwrite = TRUE)
  gs_download(subj_info, ws = "TOM", to = "data-raw/subj-info/tom.csv", overwrite = TRUE)
}

#' Save survey responses from Google Drive spreadsheet.
#'
#' @import googlesheets
save_survey_responses <- function() {
  dir.create("data-raw/survey", showWarnings = FALSE)
  gs_title("totems-survey-responses") %>%
    gs_download(ws = "Form Responses 1",
                to = "data-raw/survey/responses.csv",
                overwrite = TRUE)
}

read_subj_info <- function(sheet_name) {
  name <- paste0("data-raw/subj-info/", sheet_name, ".csv")
  readr::read_csv(name)
}

read_survey_responses <- function() {
  readr::read_csv("data-raw/survey/responses.csv")
}

all_subjs_in_info_sheets <- function() {
  tot_toi <- read_subj_info("tot-toi") %>%
    select(ID_Player = SubjID) %>%
    replace_id_player()

  tom <- read_subj_info("tom") %>%
    select(contains("_id")) %>%
    tidyr::gather(SessionName, ID_Player, -student_id) %>%
    select(ID_Player) %>%
    replace_id_player()

  subjs_in_info_sheets <- bind_rows(tot_toi, tom) %>%
    dplyr::filter(!is.na(SessionID)) %>%
    .$SessionID

  subjs_in_info_sheets
}
