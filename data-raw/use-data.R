library(devtools)
load_all()

Teams <- data_teams()
Players <- data_players()
TeamPerformance <- data_team_performance()

use_data(
  Teams,
  Players,
  TeamPerformance,
  overwrite = TRUE
)
