data_inventories <- function(con) {
  Inventories <- WorkshopAnalyzed %>%
    calculate_num_innovations() %>%
    summarize_performance_on_inventory() %>%
    calculate_difficulty()
}

calculate_num_innovations <- . %>%
  left_join(PlayerInfo) %>%
  group_by(TeamID) %>%
  arrange(TeamTime) %>%
  mutate(NumInnovations = cumsum(TeamUniqueItem)) %>%
  ungroup()

summarize_performance_on_inventory <- . %>%
  group_by(TeamID, TeamInventory, NumInnovations, NumAdjacent) %>%
  summarize(
    TeamGuesses = n(),
    TeamUniqueGuesses = sum(TeamUniqueGuess),
    UniqueGuesses = sum(UniqueGuess),
    Duration = max(TeamTime) - min(TeamTime)
  ) %>%
  ungroup() %>%
  arrange(TeamID, NumInnovations)

# The difficulty of a particular inventory is a function of the number
# of items available, which corresponds to the total number of combinations
# possible relative to the number of adjacent items that could be discovered.
calculate_difficulty <- . %>%
  mutate(Difficulty = (6 + NumInnovations)/NumAdjacent)
