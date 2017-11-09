
# Load prereqs
# load("data/Guesses.rda")
# load("data/Players.rda")

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

devtools::use_data(Inventories, overwrite = TRUE)
