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


#' Create a new column Inheritance that labels G2 Diachronic players
#' in the 50 labor minute experiment.
#'
#' Inheritance is "diachronic_inheritance" for the G2 Diachronic player,
#' and "no_inheritance" for all other session types.
#'
#' @export
highlight_inheritance_50 <- function(frame) {
  inheritance_levels <- c("diachronic_inheritance", "individual_inheritance", "no_inheritance")
  inheritance_labels <- c("Diachronic inheritance", "Individual inheritance", "No inheritance")
  highlight_inheritance_map <- expand.grid(
    Strategy = c("Diachronic", "Isolated", "Synchronic"),
    SessionDuration = 25,
    Generation = 1:2,
    stringsAsFactors = FALSE
  ) %>%
    bind_rows(list(Strategy = "Isolated", Generation = 1, SessionDuration = 50)) %>%
    dplyr::filter(!(Strategy == "Synchronic" & Generation > 1)) %>%
    dplyr::mutate(
      Inheritance = ifelse(Generation == 1, "no_inheritance",
                           ifelse(Strategy == "Diachronic", "diachronic_inheritance",
                                  ifelse(Strategy == "Isolated", "individual_inheritance", NA))),
      InheritanceOrdered = factor(Inheritance, levels = inheritance_levels, labels = inheritance_labels),
      DiachronicInheritance = Inheritance == "diachronic_inheritance"
    ) %>%
    dplyr::arrange(Strategy, Generation)

  inheritance_treat <- contr.treatment(inheritance_levels)
  highlight_inheritance_map %<>% mutate(InheritanceTreat = InheritanceOrdered)
  contrasts(highlight_inheritance_map$InheritanceTreat) <- inheritance_treat
  inheritance_treat %<>% as.data.frame()
  colnames(inheritance_treat) <- c("Diachronic_v_Individual", "Diachronic_v_NoInheritance")
  inheritance_treat %<>% tibble::rownames_to_column("Inheritance")

  highlight_inheritance_map %<>% left_join(inheritance_treat)
  if(missing(frame)) return(highlight_inheritance_map)
  left_join(frame, highlight_inheritance_map)
}

#' Create a new column Inheritance that labels G4 Diachronic players
#' in the 100 labor minute experiment.
#'
#' Inheritance is "diachronic_inheritance" for the G4 Diachronic player,
#' and "no_inheritance" for all other session types.
#'
#' @export
highlight_inheritance_100 <- function(frame) {
  inheritance_levels <- c("diachronic_inheritance", "individual_inheritance", "no_inheritance")
  inheritance_labels <- c("Diachronic inheritance", "Individual inheritance", "No inheritance")
  highlight_inheritance_map <- expand.grid(
    Strategy = c("Diachronic", "Isolated", "Synchronic"),
    Generation = 1:4,
    stringsAsFactors = FALSE
  ) %>%
    dplyr::filter(!(Strategy == "Synchronic" & Generation > 1)) %>%
    dplyr::mutate(
      Inheritance = ifelse(Generation == 1, "no_inheritance",
                           ifelse(Strategy == "Diachronic", "diachronic_inheritance",
                                  ifelse(Strategy == "Isolated", "individual_inheritance", NA))),
      InheritanceOrdered = factor(Inheritance, levels = inheritance_levels, labels = inheritance_labels),
      DiachronicInheritance = Inheritance == "diachronic_inheritance"
    ) %>%
    dplyr::arrange(Strategy, Generation)

  inheritance_treat <- contr.treatment(inheritance_levels)
  highlight_inheritance_map %<>% mutate(InheritanceTreat = InheritanceOrdered)
  contrasts(highlight_inheritance_map$InheritanceTreat) <- inheritance_treat
  inheritance_treat %<>% as.data.frame()
  colnames(inheritance_treat) <- c("Diachronic_v_Individual", "Diachronic_v_NoInheritance")
  inheritance_treat %<>% tibble::rownames_to_column("Inheritance")

  highlight_inheritance_map %<>% left_join(inheritance_treat)

  if(missing(frame)) return(highlight_inheritance_map)
  left_join(frame, highlight_inheritance_map)
}

#' Create a session type column that compares individual session types
#' for the 50 labor minute experiment.
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
recode_session_type_50min <- function(frame) {
  session_type_levels <- c(
    "DG1", "DG2",
    "I50",
    "S2"
  )

  session_type_labels <- c(
    "Diachronic G1",
    "Diachronic G2",
    "Isolated 50min",
    "Synchronic 2"
  )

  session_type_map <- data_frame(
    Strategy = c(rep("Diachronic", 2), "Isolated", "Synchronic"),
    Generation = c(1:2, 1, 1),
    SessionDuration = c(rep(25, 2), 50, 25),
    NumPlayers = c(rep(2, 2), 1, 2),
    SessionType = session_type_levels,
    SessionTypeOrdered = factor(session_type_levels, levels = session_type_levels, labels = session_type_labels),
    SessionTypeSimple = factor(session_type_levels, levels = session_type_levels, labels = session_type_levels),
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
    tibble::rownames_to_column("SessionType")
  session_type_map <- left_join(session_type_map, session_type_treat_contrasts)

  if(missing(frame)) return(session_type_map)
  left_join(frame, session_type_map)
}

#' Create a session type column that compares individual session types
#' for the 100 labor minute experiment.
#'
#' Session types are:
#' - Diachronic G1
#' - Diachronic G2
#' - Diachronic G3
#' - Diachronic G4
#' - Isolated S1
#' - Isolated S2
#' - Isolated S3
#' - Isolated S4
#' - Synchronic 4
#'
#' @export
recode_session_type_100 <- function(frame) {
  session_type_levels <- c(
    "DG1", "DG2", "DG3", "DG4",
    "IS1", "IS2", "IS3", "IS4",
    "S4"
  )

  session_type_labels <- c(
    "DG1",
    "DG2",
    "DG3",
    "DG4",
    "IS1",
    "IS2",
    "IS3",
    "IS4",
    "S4"
  )

  session_type_map <- data_frame(
    Strategy = c(rep("Diachronic", 4), rep("Isolated", 4), "Synchronic"),
    Generation = c(1:4, 1:4, 1),
    SessionDuration = 25,
    NumPlayers = c(rep(4, 4), rep(1, 4), 4),
    SessionType = session_type_levels,
    SessionTypeOrdered = factor(session_type_levels, levels = session_type_levels, labels = session_type_labels),
    SessionTypeTreat = factor(session_type_levels, levels = session_type_levels)
  )

  # Set treatment contrasts for SessionType with DG4 as base comparison group.
  session_type_treat_contrasts <- contr.treatment(factor(session_type_levels, levels = session_type_levels),
                                                  base = 4)
  contrasts(session_type_map$SessionTypeTreat) <- session_type_treat_contrasts

  session_type_treat_contrasts <- as.data.frame(session_type_treat_contrasts)
  contrast_names <- colnames(session_type_treat_contrasts) %>%
    purrr::map(function(x) paste0("DG4_v_", x)) %>%
    unlist()
  colnames(session_type_treat_contrasts) <- contrast_names
  session_type_treat_contrasts <- session_type_treat_contrasts %>%
    tibble::rownames_to_column("SessionType")
  session_type_map <- left_join(session_type_map, session_type_treat_contrasts)

  if(missing(frame)) return(session_type_map)
  left_join(frame, session_type_map)
}

#' Recode generation type for 50 labor minute teams.
#' @export
recode_generation_type <- function(frame) {
  generation_type_levels <- c("G1", "G2", "S1", "S2")
  generation_type_labels <- generation_type_levels
  generation_type_map <- data_frame(
    Strategy = c(rep("Diachronic", 2), rep("Isolated", 2)),
    Generation = rep(1:2, 2),
    GenerationType = generation_type_levels,
    GenerationTypeLabel = factor(generation_type_levels, levels = generation_type_levels, labels = generation_type_labels)
  )
  if(missing(frame)) return(generation_type_map)
  left_join(frame, generation_type_map)
}

#' @export
recode_discovered <- function(frame) {
  levels <- c(TRUE, FALSE)
  types <- c("discovered", "undiscovered")
  labels <- c("Discovered", "Undiscovered")
  long_labels <- c("Item discovered", "Item never discovered")
  map <- data_frame(
    Discovered = levels,
    DiscoveredType = types,
    DiscoveredLabel = factor(levels, levels = levels, labels = labels),
    DiscoveredLong = factor(levels, levels = levels, labels = long_labels)
  )
  if(missing(frame)) return(map)
  left_join(frame, map)
}


#' @export
recode_inheritance <- function(frame) {
  inheritance_levels <- c("no_inheritance", "diachronic_inheritance", "individual_inheritance")
  map <- data_frame(Inheritance = inheritance_levels)

  inheritance_treat <- contr.treatment(inheritance_levels) %>% as.data.frame()
  colnames(inheritance_treat) <- c("Diachronic_v_Individual", "Diachronic_v_NoInheritance")
  inheritance_treat %<>% tibble::rownames_to_column("Inheritance")

  map %<>% left_join(inheritance_treat)
  if(missing(frame)) return(map)
  left_join(frame, map)
}
