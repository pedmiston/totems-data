# Save the csvs from the Totems experiment as rda data files in the R pkg

library(devtools)
library(tidyverse)
library(lazyeval)
library(magrittr)
library(lubridate)
# Don't load "totems" or use_data() might not work properly
source("R/util.R")
source("R/time-bins.R")

# Start here!
read_tables

source("data-raw/deidentify-totems-data.R")    # Deidentify totems data
source("data-raw/filter-valid-totems-data.R")  # Filter valid players and teams

# TeamInfo ---------------------------------------------------------------------
TeamInfo <- Group %>%
  rename(Strategy = Treatment, TeamSize = Size) %>%
  select(TeamID, Strategy, TeamSize)

# PlayerInfo -------------------------------------------------------------------
PlayerInfo <- Player %>%
  left_join(Group) %>%
  rename(Strategy = Treatment, SessionDuration = BuildingTime) %>%
  mutate(Generation = ifelse(Strategy != "Diachronic", 1, Ancestor)) %>%
  select(PlayerID, TeamID, Strategy, Generation, SessionDuration) %>%
  arrange(Strategy, TeamID, Generation) %>%
  # Assign player indices to each player
  group_by(TeamID, Generation) %>%
  mutate(PlayerIX = paste0("P", 1:n())) %>%
  ungroup()


# Guesses ----------------------------------------------------------------------
Guesses <- WorkshopAnalyzed %>%
  rename(Guess = WorkShopString, Result = WorkShopResult) %>%
  left_join(PlayerInfo) %>%
  count_guesses("PlayerID", "GuessNum") %>%
  count_guesses("TeamID", "TeamGuessNum") %>%
  select(
    PlayerID,
    PlayerTime, TeamTime,
    GuessNum, TeamGuessNum,
    Guess, Result,
    Score, TeamScore,
    UniqueGuess, TeamUniqueGuess,
    UniqueItem, TeamUniqueItem
  )

# Inventories ------------------------------------------------------------------
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

Inventories <- WorkshopAnalyzed %>%
  calculate_num_innovations() %>%
  summarize_performance_on_inventory() %>%
  calculate_difficulty()

# Performance ------------------------------------------------------------------
TeamPerformance <- Guesses %>%
  left_join(PlayerInfo) %>%
  # !!! Exclude diachronic players with generations > 2
  filter(Generation <= 2) %>%
  # !!! Exclude synchronic teams with team size == 4
  # ...
  group_by(TeamID) %>%
  summarize(
    NumInnovations = sum(TeamUniqueItem),
    NumGuesses = max(TeamGuessNum),
    NumUniqueGuesses = sum(TeamUniqueGuess)
  ) %>%
  # Add in Score from Player table
  left_join(
    Player %>%
      left_join(PlayerInfo) %>%
      filter(Generation <= 2) %>%
      group_by(TeamID) %>%
      summarize(Score = max(Score))
  ) %>%
  left_join(TeamInfo)

PlayerPerformance <- Guesses %>%
  group_by(PlayerID) %>%
  summarize(
    NumInnovations = sum(UniqueItem),
    NumGuesses = max(GuessNum),
    NumUniqueGuesses = sum(UniqueGuess),
    NumTeamUniqueGuesses = sum(TeamUniqueGuess)
  ) %>%
  # Add in Score from Player table
  left_join(select(Player, PlayerID, Score))

# SampledPerformance -----------------------------------------------------------
calculate_rolling_team_performance <- . %>%
  group_by(TeamID) %>%
  arrange(TeamTime) %>%
  mutate(
    NumTeamInnovations = cumsum(TeamUniqueItem),
    TeamScore = cumsum(Score)
  ) %>%
  ungroup()

calculate_rolling_player_performance <- . %>%
  group_by(PlayerID) %>%
  arrange(TeamTime) %>%
  mutate(
    NumInnovations = cumsum(UniqueItem),
    Score = cumsum(Score)
  ) %>%
  ungroup()

SampledPerformance <- Guesses %>%
  left_join(PlayerInfo) %>%
  calculate_rolling_team_performance() %>%
  calculate_rolling_player_performance() %>%
  group_by(PlayerID) %>%
  # Sample closest trial every 60 seconds
  do({ get_closest_trials_to_times(., times = seq(0, 50 * 60, by = 60)) }) %>%
  ungroup() %>%
  # Prevent Synchronic teams from being sampled outside their range.
  filter(!(Strategy == "Synchronic" & SampledTime > 25*60)) %>%
  # Calculate number of guesses in each time bin
  group_by(PlayerID) %>%
  mutate(
    NewGuesses = GuessNum - lag(GuessNum),
    NewTeamGuesses = TeamGuessNum - lag(TeamGuessNum)
  ) %>%
  ungroup() %>%
  group_by(PlayerID, SampledTime) %>%
  summarize(
    # Take max() of cumsum variables, not mean()
    NumInnovations = max(NumInnovations),
    NumTeamInnovations = max(NumTeamInnovations),
    Score = max(Score),
    TeamScore = max(TeamScore),
    # Since time step is 60 seconds,
    GuessesPerMinute = max(NewGuesses),
    TeamGuessesPerMinute = max(NewTeamGuesses)
  ) %>%
  ungroup() %>%
  left_join(PlayerInfo) %>%
  mutate(
    SampledPlayerTime = ifelse(Strategy != "Diachronic", SampledTime,
                               SampledTime - (Generation - 1) * (25 * 60))
  ) %>%
  select(
    PlayerID, SampledTime, SampledPlayerTime,
    NumInnovations, NumTeamInnovations,
    Score, TeamScore,
    GuessesPerMinute, TeamGuessesPerMinute
  )

# Save data to package ---------------------------------------------------------
use_data(
  TeamInfo,
  PlayerInfo,
  Guesses,
  Inventories,
  TeamPerformance,
  PlayerPerformance,
  SampledPerformance,
  overwrite = TRUE
)
