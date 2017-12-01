library(dplyr)
devtools::load_all()

data("ValidSessions")

ValidSessions %>%
  mutate(ID_Player = as.numeric(stringr::str_replace(SessionID, "S", ""))) %>%
  arrange(Exp, ID_Player) %>%
  select(Exp, ID_Player, SessionStatus, TeamStatus) %>%
  write.csv("data-raw/valid-player-ids.csv", row.names = FALSE)
