data("Teams")
data("Players")
data("Guesses")

context("Verify valid teams")

filter_valid_teams <- . %>% dplyr::filter(TeamStatus == "V") %>% .$TeamID %>% unique()

team_ids <- filter_valid_teams(Teams)
team_ids_in_guesses <- filter_valid_teams(Guesses)
team_ids_in_players <- filter_valid_teams(Players)

test_that("TeamIDs in Teams are also in Players", {
  expect_true(setequal(team_ids, team_ids_in_players))
})

test_that("TeamIDs in Teams are also in Guesses", {
  expect_true(setequal(team_ids, team_ids_in_guesses))
})

context("Verify valid sessions")

filter_valid_sessions <- . %>% dplyr::filter(SessionStatus == "V") %>% .$SessionID %>% unique()

session_ids <- filter_valid_sessions(Players)
session_ids_in_guesses <- filter_valid_sessions(Guesses)

test_that("SessionIDs in Players are also in Guesses", {
  expect_true(setequal(session_ids, session_ids_in_guesses))
})
