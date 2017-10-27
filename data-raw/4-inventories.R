Inventories <- Guesses %>%
  group_by(SessionID, SessionInventoryID) %>%
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
  rename(InventoryID = SessionInventoryID) %>%
  join_players()
