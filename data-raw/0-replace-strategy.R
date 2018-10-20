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

# Take the TOP subjects,
# Create a new group for them and who they inherited from
top_map <- read_subj_info("tot-fall-18") %>%
  dplyr::filter(SubjID > 900) %>%
  select(SubjID, InheritedID)

top_groups <- read_table("Table_Player") %>%
  dplyr::filter(ID_Player %in% top_map$SubjID) %>%
  select(SubjID = ID_Player, ID_Group) %>%
  mutate(ID_Group_New = paste0(ID_Group, "-", SubjID))

# Change group in Table_Player
top_group_map <- top_groups$ID_Group_New
names(top_group_map) <- top_groups$SubjID

read_table("Table_Player") %>%
  dplyr::mutate(ID_Group = ifelse(ID_Player %in% top_map$SubjID, top_group_names[as.character(top_map$SubjID)], ID_Group))

left_join(top_map, top_groups)


top_inherited_ids <- read_subj_info("tot-fall-18") %>%
  dplyr::filter(SubjID > 900) %>%
  select(SessionID = InheritedID) %>%
  .$SessionID %>%
  .[!is.na(.)]

Players <- read_table("Table_Player") %>%
  dplyr::filter(ID_Player %in% top_subj_ids) %>%
  mutate(ID_Group = paste0(ID_Group, "-", ID_Player))
