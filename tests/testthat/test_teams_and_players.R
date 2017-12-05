data("Sessions")
data("Guesses")

context("Verify valid sessions")

filter_valid_sessions <- . %>% dplyr::filter(SessionStatus == "valid") %>% .$SessionID %>% unique()

session_ids <- filter_valid_sessions(Sessions)
session_ids_in_guesses <- filter_valid_sessions(Guesses)

test_that("SessionIDs in Players are also in Guesses", {
  expect_true(setequal(session_ids, session_ids_in_guesses))
})
