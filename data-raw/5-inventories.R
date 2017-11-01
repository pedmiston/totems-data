
# Load prereqs
# load("data/Guesses.rda")
# load("data/Players.rda")

Inventories <- Guesses %>%
  group_by(SessionID, PrevSessionInventoryID) %>%
  summarize(
    NumGuesses = n(),
    UniqueGuesses = sum(UniqueSessionGuess),
    RepeatGuesses = NumGuesses - UniqueGuesses,
    RepeatResults = sum(Result != 0),
    StartTime = min(SessionTime),
    EndTime = max(SessionTime),
    SessionDuration = EndTime - StartTime,
    Result = ifelse(sum(UniqueSessionResult) == 0, 0,
                    Result[UniqueSessionResult == 1])
  ) %>%
  ungroup() %>%
  rename(InventoryID = PrevSessionInventoryID) %>%
  arrange(SessionID, StartTime) %>%
  left_join(Players)

devtools::use_data(Inventories, overwrite = TRUE)
