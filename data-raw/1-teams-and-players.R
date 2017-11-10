Teams <- read_table("Table_Group") %>%
  rename(Strategy = Treatment, SessionDuration = BuildingTime) %>%
  replace_id_group() %>%
  label_players_per_session() %>%
  label_team_conditions() %>%
  label_team_status() %>%
  select(
    Exp,
    TeamID,
    Strategy,
    NumPlayers,
    SessionsPerPlayer,
    PlayersPerSession,
    SessionDuration,
    TeamStatus
  ) %>%
  arrange(TeamID, desc(Exp))

Players <- read_table("Table_Player") %>%
  replace_id_group() %>%
  label_session_duration() %>%
  label_strategy() %>%
  label_players_per_session() %>%
  replace_id_player() %>%
  replace_ancestor() %>%
  label_player_conditions() %>%
  label_session_status() %>%
  label_team_status() %>%
  select(
    Exp,
    PlayerID,
    PlayerIX,
    SessionID,
    SessionIX,
    TeamID,
    Strategy,
    NumPlayers,
    Generation,
    SessionDuration,
    SessionStatus,
    TeamStatus
  ) %>%
  arrange(TeamID, desc(Exp), PlayerIX, SessionIX, Strategy)

ValidSessions <- select(Players, Exp, PlayerID, SessionID, SessionStatus, TeamStatus)
ValidTeams <- select(Teams, Exp, TeamID, TeamStatus)

# Process survey responses

Survey <- readr::read_csv("data-raw/survey/responses.csv")
SurveyQuestions <- data_frame(
  QuestionText = colnames(Survey)[3:length(colnames(Survey))],
  QuestionID = paste0("Q", seq_along(QuestionText)),
  QuestionTag = c("recap", "instructions", "familiar", "enjoyable", "difficulty", "strategy", "comments")
) %>%
  select(QuestionID, QuestionTag, QuestionText)

Survey <- readr::read_csv("data-raw/survey/responses.csv") %>%
  tidyr::gather(QuestionText, Response, -(Timestamp:`Participant ID`)) %>%
  rename(ID_Player = `Participant ID`) %>%
  left_join(SurveyQuestions) %>%
  select(ID_Player, QuestionID, QuestionTag, Response, QuestionText, Timestamp)

# Merge survey info with players
Familiarity <- Survey %>%
  dplyr::filter(QuestionTag == "familiar") %>%
  replace_id_player() %>%
  transmute(PlayerID, Familiarity = as.numeric(Response))

nrow_before <- nrow(Players)
Players <- left_join(Players, Familiarity)
stopifnot(nrow(Players) == nrow_before)

TeamFamiliarity <- Players %>%
  group_by(Exp, TeamID) %>%
  summarize(Familiarity = mean(Familiarity)) %>%
  ungroup()

nrow_before <- nrow(Teams)
Teams <- left_join(Teams, TeamFamiliarity)
stopifnot(nrow(Teams) == nrow_before)

TeamDiversity50 <- Players %>%
  dplyr::filter(
    Exp == "50LaborMinutes",
    Strategy != "Isolated"
  ) %>%
  select(Exp, TeamID, PlayerID, PlayerIX, Familiarity) %>%
  group_by(Exp, TeamID) %>%
  do({
    diversity <- abs(.data$Familiarity[1] - .data$Familiarity[2])
    if(is.na(diversity)) {
      more_experienced_player_id <- NA
    } else if(diversity == 0) {
      more_experienced_player_id <- .data$PlayerID[1]
    } else {
      more_experienced_player_id <- .data$PlayerID[.data$Familiarity == max(.data$Familiarity)]
    }
    data_frame(
      Diversity = diversity,
      MostExperiencedPlayerID = more_experienced_player_id
    )
  }) %>%
  ungroup() %>%
  dplyr::filter(!is.na(Diversity))

PlayerGenerations <- Players %>%
  dplyr::filter(Strategy == "Diachronic") %>%
  select(
    Exp,
    MostExperiencedPlayerID = PlayerID,
    MostExperiencedPlayerGeneration = Generation
  ) %>%
  unique()

PlayerGenerations50 <- PlayerGenerations %>%
  mutate(Exp = "50LaborMinutes")

nrow_before <- nrow(TeamDiversity50)
TeamDiversity50 <- left_join(TeamDiversity50, PlayerGenerations)
stopifnot(nrow(TeamDiversity50) == nrow_before)

nrow_before <- nrow(Teams)
Teams <- left_join(Teams, TeamDiversity50)
stopifnot(nrow(Teams) == nrow_before)

devtools::use_data(Teams, Players,
                   ValidTeams, ValidSessions,
                   Survey, SurveyQuestions,
                   overwrite = TRUE)
