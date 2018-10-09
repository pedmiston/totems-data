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
