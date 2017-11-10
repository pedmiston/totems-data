# load("data/Guesses.rda")
# load("data/Teams.rda")

TeamPerformance <- Guesses %>%
  recode_guess_type("UniqueTeamGuess", "UniqueTeamResult") %>%
  group_by(Exp, Strategy, SessionDuration, TeamID) %>%
  summarize(
    NumGuesses = max(NumTeamGuess),
    NumRedundantGuesses = sum(GuessType == "redundant"),
    NumRepeatedItems = sum(GuessType == "repeat_item"),
    NumUniqueGuesses = sum(GuessType == "unique_guess"),
    NumInnovations = sum(UniqueTeamResult)
  ) %>%
  ungroup() %>%
  left_join(Teams) %>%
  label_team_status()

PlayerPerformance <- Guesses %>%
  recode_guess_type("UniqueSessionGuess", "UniqueSessionResult") %>%
  group_by(Exp, SessionID) %>%
  summarize(
    NumGuesses = max(NumSessionGuess),
    NumRedundantGuesses = sum(GuessType == "redundant"),
    NumRepeatedItems = sum(GuessType == "repeat_item"),
    NumUniqueGuesses = sum(GuessType == "unique_guess"),
    NumInnovations = sum(UniqueSessionResult)
  ) %>%
  ungroup() %>%
  left_join(Players) %>%
  label_team_status() %>%
  label_session_status()

Inventories <- Guesses %>%
  group_by(Exp, SessionID, SessionDuration, PrevSessionInventoryID) %>%
  summarize(
    NumGuesses = n(),
    UniqueGuesses = sum(UniqueSessionGuess),
    RepeatGuesses = NumGuesses - UniqueGuesses,
    RepeatResults = sum(Result != 0),
    StartTime = min(SessionTime),
    EndTime = max(SessionTime),
    Result = ifelse(sum(UniqueSessionResult) == 0, 0,
                    Result[UniqueSessionResult == 1]),
    ProblemTime = ifelse(Result != 0, EndTime - StartTime, NA)
  ) %>%
  ungroup() %>%
  rename(InventoryID = PrevSessionInventoryID) %>%
  arrange(SessionID, StartTime) %>%
  left_join(Players)

devtools::use_data(TeamPerformance, PlayerPerformance, Inventories, overwrite = TRUE)
