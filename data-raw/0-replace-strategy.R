# Replace the Strategy of TTP subjects
ttp_session_ids <- read_subj_info("ttp") %>%
  select(SessionID = SubjID) %>%
  .$SessionID %>%
  .[!is.na(.)]
ttp_group_ids <- read_table("Table_Player") %>%
  dplyr::filter(ID_Player %in% ttp_session_ids) %>%
  .$ID_Group
Groups <- read_table("Table_Group") %>%
  dplyr::mutate(
    Treatment = ifelse(ID_Group %in% ttp_group_ids, "Diachronic", Treatment)
  )
write.csv(Groups, "data-raw/tables/Table_Group.csv", row.names = FALSE)

# Identify TOT subjects who inherited from TOM subjects
tot_subj_info <- read_subj_info("tot-fall-18") %>%
  dplyr::filter(SubjID > 900)

tot_map <- tot_subj_info %>%
  select(ID_Player = SubjID, InheritedID)

tot_groups <- read_table("Table_Player") %>%
  dplyr::filter(ID_Player %in% tot_map$ID_Player) %>%
  dplyr::left_join(tot_map) %>%
  mutate(ID_Group_New = paste0(ID_Group, "-", InheritedID)) %>%
  select(ID_Player, ID_Group, InheritedID, ID_Group_New)

# Take the TOP subjects who inherited from TOM subjects and move them to new groups.
new_tot_groups <- select(tot_groups, ID_Group = ID_Group_New) %>%
  mutate(Size = 2, Open = 0, Treatment = "Diachronic", BuildingTime = 25, Status = "E")
read_table("Table_Group") %>%
  bind_rows(new_tot_groups) %>%
  write.csv("data-raw/tables/Table_Group.csv", row.names = FALSE)

# Change group in Table_Player
tot_group_map <- tot_groups$ID_Group_New
names(tot_group_map) <- tot_groups$ID_Player

read_table("Table_Player") %>%
  dplyr::mutate(ID_Group = ifelse(ID_Player %in% names(tot_group_map),
                                  tot_group_map[as.character(ID_Player)], ID_Group)) %>%
  write.csv("data-raw/tables/Table_Player.csv", row.names = FALSE)
