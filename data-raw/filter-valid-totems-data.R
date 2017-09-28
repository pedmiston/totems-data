# Select valid participants
# Valid participants are those recorded in the subject info sheet
valid_participant_ids <- SubjInfo$PlayerID
filter_valid_participants <- . %>% filter(PlayerID %in% valid_participant_ids)

Player   %<>% filter_valid_participants()
Workshop %<>% filter_valid_participants()
WorkshopAnalyzed %<>% filter_valid_participants()
Survey   %<>% filter_valid_participants()

# Select valid teams
# Valid teams are those with Status == "E" in the Table_Group
verified_teams <- Group %>% filter(Status == "E") %>% .$TeamID
filter_valid_teams <- . %>%
  filter(TeamID %in% verified_teams)

Group    %<>% filter_valid_teams()
Player   %<>% filter_valid_teams()
