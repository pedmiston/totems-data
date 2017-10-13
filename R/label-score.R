scores <- readr::read_csv("data-raw/scores.csv") %>%
  select(Guess, Result, Score)

label_score <- function(frame) {
  frame %>%
    label_session_score() %>%
    label_player_score() %>%
    label_team_score()
}

label_session_score <- function(frame) {
  scores <- scores %>%
    rename(SessionScore = Score) %>%
    mutate(UniqueSessionResult = TRUE)
  left_join(frame, scores) %>%
    tidyr::replace_na(list(SessionScore = 0))
}

#' Score guesses that resulted in unique items
label_player_score <- function(frame) {
  scores <- scores %>%
    rename(PlayerScore = Score) %>%
    mutate(UniquePlayerResult = TRUE)
  left_join(frame, scores) %>%
    tidyr::replace_na(list(PlayerScore = 0))
}

#' Score guesses that resulted in unique team items
label_team_score <- function(frame) {
  scores <- scores %>%
    rename(TeamScore = Score) %>%
    mutate(UniqueTeamResult = TRUE)
  left_join(frame, scores) %>%
    tidyr::replace_na(list(TeamScore = 0))
}
