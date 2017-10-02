library(devtools)
load_all()

con <- connect_db()
Teams <- data_teams(con)
Players <- data_players(con)

use_data(Teams, Players, overwrite = TRUE)
