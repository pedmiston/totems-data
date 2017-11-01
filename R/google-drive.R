#' @import googlesheets
get_subj_info <- function() {
  subj_info <- gs_title("totems-subj-info")
  gs_download(subj_info, ws = "TOT/TOI", to = "data-raw/subj-info/tot-toi.csv", overwrite = TRUE)
  gs_download(subj_info, ws = "TOM", to = "data-raw/subj-info/tom.csv", overwrite = TRUE)
}

#' @import googlesheets
get_survey_responses <- function() {
  gs_title("totems-survey-responses") %>%
    gs_download(ws = "Form Responses 1",
                to = "data-raw/survey/survey-responses.csv",
                overwrite = TRUE)
}
