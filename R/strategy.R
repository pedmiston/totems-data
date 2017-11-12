#' Recode Strategy
#'
#' @import dplyr
#' @import magrittr
#' @export
recode_strategy <- function(frame) {
  strategies <- c("Synchronic", "Isolated", "Diachronic")
  map <- data_frame(Strategy = strategies,
                    StrategyLabel = factor(strategies, levels = strategies))

  # Add treatment contrasts
  treatment_contrasts <- contr.treatment(strategies, base = 3) %>%
    as.data.frame() %>%
    rename(Diachronic_v_Synchronic = Synchronic, Diachronic_v_Isolated = Isolated) %>%
    mutate(Strategy = row.names(.))
  map %<>% left_join(treatment_contrasts)

  # Add helmert contrasts
  helmert_contrasts <- contr.helmert(3) %>%
    as.data.frame() %>%
    rename(DvS = V1, DSvI = V2) %>%
    mutate(Strategy = c("Diachronic", "Synchronic", "Isolated"))
  map %<>% left_join(helmert_contrasts)

  if (missing(frame)) return(map)
  left_join(frame, map)
}


#' Create a new column Inheritance that labels G2-G4 Diachronic players
#' as "diachronic_inheritance" and everyone else as "no_inheritance".
#'
#' @export
highlight_inheritance <- function(frame) {
  highlight_inheritance_map <- expand.grid(
    Strategy = c("Diachronic", "Isolated", "Synchronic"),
    Generation = 1:4,
    stringsAsFactors = FALSE
  ) %>%
    dplyr::filter(!(Strategy == "Synchronic" & Generation > 1)) %>%
    dplyr::mutate(Inheritance = ifelse(Strategy == "Diachronic" & Generation > 1,
                                       "diachronic_inheritance", "no_inheritance")) %>%
    dplyr::arrange(Strategy, Generation)
  if(missing(frame)) return(highlight_inheritance_map)
  left_join(frame, highlight_inheritance_map)
}

#' Create a session type column that compares individual session types.
#'
#' Session types are:
#' - Diachronic G1
#' - Diachronic G2
#' - Isolated 50min
#' - Isolated S1
#' - Isolated S2
#' - Synchronic 2
#'
#' @export
recode_session_type_50 <- function(frame) {
  session_type_levels <- c(
    "DG1", "DG2",
    "I50", "IS1", "IS2",
    "S2"
  )

  session_type_labels <- c(
    "Diachronic G1",
    "Diachronic G2",
    "Isolated 50min",
    "Isolated S1",
    "Isolated S2",
    "Synchronic 2"
  )

  session_type_map <- data_frame(
    Strategy = c(rep("Diachronic", 2), rep("Isolated", 3), "Synchronic"),
    Generation = c(1:2, 1, 1:2, 1),
    SessionDuration = c(rep(25, 2), 50, rep(25, 3)),
    NumPlayers = c(rep(2, 2), rep(1, 3), 2),
    SessionType = session_type_levels,
    SessionTypeOrdered = factor(session_type_levels, levels = session_type_levels, labels = session_type_labels),
    SessionTypeTreat = factor(session_type_levels, levels = session_type_levels)
  )

  # Set treatment contrasts for SessionType with D-G2 as base comparison group.
  session_type_treat_contrasts <- contr.treatment(factor(session_type_levels, levels = session_type_levels),
                                                  base = 2)
  contrasts(session_type_map$SessionTypeTreat) <- session_type_treat_contrasts

  session_type_treat_contrasts <- as.data.frame(session_type_treat_contrasts)
  contrast_names <- colnames(session_type_treat_contrasts) %>%
    purrr::map(function(x) paste0("DG2_v_", x)) %>%
    unlist()
  colnames(session_type_treat_contrasts) <- contrast_names
  session_type_treat_contrasts <- session_type_treat_contrasts %>%
    rownames_to_column("SessionType")
  session_type_map <- left_join(session_type_map, session_type_treat_contrasts)

  if(missing(frame)) return(session_type_map)
  left_join(frame, session_type_map)
}

#' Recode generation type for 50 labor minute teams.
#' @export
recode_generation_type <- function(frame) {
  generation_type_levels <- c("G1", "G2", "S1", "S2")
  generation_type_labels <- c("Generation 1", "Generation 2", "Session 1", "Session 2")
  generation_type_map <- data_frame(
    Strategy = c(rep("Diachronic", 2), rep("Isolated", 2)),
    Generation = rep(1:2, 2),
    GenerationType = generation_type_levels,
    GenerationTypeLabel = factor(generation_type_levels, levels = generation_type_levels, labels = generation_type_labels)
  )
  if(missing(frame)) return(generation_type_map)
  left_join(frame, generation_type_map)
}
