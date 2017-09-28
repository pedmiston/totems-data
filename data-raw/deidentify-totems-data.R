# Remove datetime information from subject info sheet and survey data
SubjInfo %<>% select(-Date, -Room)
Survey %<>% select(-Timestamp)

# Remove datetime information from group identifier
team_id_levels <- factor(Group$ID_Group) %>% levels()
team_id_labels <- paste0("G", seq_along(team_id_levels))
team_id_map <- data_frame(
  ID_Group = team_id_levels,
  TeamID = team_id_labels
)
deidentify_group_id <- . %>% left_join(team_id_map) %>% select(-ID_Group)

Group    %<>% deidentify_group_id()
Player   %<>% deidentify_group_id()

# Recode player id
# Create a new PlayerID variable that is a character instead of a number.
player_id_levels <- unique(Player$ID_Player)
player_id_labels <- paste0("P", seq_along(player_id_levels))
player_id_map <- data_frame(
  ID_Player = player_id_levels,
  PlayerID = player_id_labels
)
recode_player_id <- . %>% left_join(player_id_map) %>% select(-ID_Player)

SubjInfo %<>% recode_player_id()
Player   %<>% recode_player_id()
Workshop %<>% recode_player_id()
WorkshopAnalyzed %<>% recode_player_id()
Survey %<>% rename(ID_Player = `Participant ID`) %>% recode_player_id()
