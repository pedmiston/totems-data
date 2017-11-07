library(ggplot2)

# DEBUG: Why is NumUniqueSessionResults != MaxInventorySize?
data("Guesses")

Guesses <- Guesses %>%
  dplyr::filter(SessionStatus == "V")

num_unique_session_results <- Guesses %>%
  group_by(Exp, SessionID) %>%
  summarize(SumUniqueSessionResults = sum(UniqueSessionResult))

inventory_sizes <- Guesses %>%
  group_by(Exp, SessionID) %>%
  summarize(MaxSessionInventorySize = max(SessionInventorySize))

result <- merge(num_unique_session_results, inventory_sizes)

ggplot(result) +
  aes(SumUniqueSessionResults, MaxSessionInventorySize) +
  geom_point()

result <- result %>%
  label_session_player() %>%
  label_team_id() %>%
  label_strategy()

ggplot(result) +
  aes(SumUniqueSessionResults, I(MaxSessionInventorySize - 6)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  facet_wrap("Strategy")

mismatch <- result %>%
  dplyr::filter(SumUniqueSessionResults != MaxSessionInventorySize - 6)

View(mismatch)

S115 <- dplyr::filter(Guesses, SessionID == "S115")
sum(S115$UniqueSessionResult)
max(S115$SessionInventorySize)
View(S115)

# The last guess for all sessions with a mismatch was a unique guess
dplyr::filter(Guesses, SessionID %in% mismatch$SessionID) %>%
  group_by(SessionID) %>%
  do({ tail(., n = 1) }) %>%
  .$UniqueSessionResult

# The last guess for all sessions without a mismatch was NOT a unique guess
dplyr::filter(Guesses, !(SessionID %in% mismatch$SessionID)) %>%
  group_by(SessionID) %>%
  do({ tail(., n = 1) }) %>%
  .$UniqueSessionResult
