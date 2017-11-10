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
  rename(ID_Participant = `Participant ID`) %>%
  left_join(SurveyQuestions) %>%
  select(ID_Participant, QuestionID, QuestionTag, Response, QuestionText, Timestamp)

# Merge survey info with players and teams

ValidSessions <- select(Players, Exp, PlayerID, SessionID, SessionStatus, TeamStatus)
ValidTeams <- select(Teams, Exp, TeamID, TeamStatus)

devtools::use_data(Teams, Players,
                   ValidTeams, ValidSessions,
                   Survey,
                   overwrite = TRUE)
