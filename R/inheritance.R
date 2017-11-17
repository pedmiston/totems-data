#' Label Inheritance from Generation and Strategy.
#'
#' @export
label_inheritance <- function(frame) {
  dplyr::mutate(frame,
                Inheritance = ifelse(Generation == 1, "no_inheritance",
                                     ifelse(Strategy == "Diachronic", "diachronic_inheritance",
                                            ifelse(Strategy == "Isolated", "individual_inheritance", NA))))
}

#' @export
recode_inheritance <- function(frame) {
  inheritance_levels <- c("no_inheritance", "diachronic_inheritance", "individual_inheritance")
  map <- data_frame(
    Inheritance = inheritance_levels,
    DiachronicInheritance = Inheritance == "diachronic_inheritance"
  )

  inheritance_treat <- contr.treatment(inheritance_levels) %>% as.data.frame()
  colnames(inheritance_treat) <- c("Diachronic_v_Individual", "Diachronic_v_NoInheritance")
  inheritance_treat %<>% tibble::rownames_to_column("Inheritance")

  map %<>% left_join(inheritance_treat)
  if(missing(frame)) return(map)
  left_join(frame, map)
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


#' Label the stage of the guesser's inventory relative to the team inventory.
label_stage <- function(Guesses) {
  Guesses %>%
    arrange(SessionTime) %>%
    group_by(SessionID) %>%
    mutate(Stage = ifelse(nchar(PrevTeamInventoryID) > nchar(PrevSessionInventoryID),
                          "learning", "playing")) %>%
    ungroup() %>%
    mutate(Stage == ifelse(Strategy == "Synchronic", "playing", Stage))
}

#' Label stage ix for individual players inheriting either
#' from self or another teammate.
#' @export
label_stage_ix <- function(IndividualGuesses) {
  IndividualGuesses %>%
    group_by(SessionID, Stage) %>%
    do({
      stage <- .data$Stage[[1]]
      if(stage == "learning") {
        result <- .data %>%
          arrange(desc(SessionTime)) %>%
          mutate(StageIX = -cumsum(UniqueSessionResult))
      } else if(stage == "playing") {
        result <- .data %>%
          arrange(SessionTime) %>%
          mutate(StageIX = cumsum(UniqueSessionResult))
      }
    }) %>%
    ungroup()
}

#' @export
recode_generation_type_100 <- function(frame) {
  generation_type_levels <- c("GN", "GN_1", "IN", "IN_1")
  generation_type_labels <- c("Generation N", "Generation N+1", "Session N", "Session N+1")
  generation_type_map <- data_frame(
    Strategy = rep(c("Diachronic", "Isolated"), each = 6),
    Generation = rep(c(1, 2, 2, 3, 3, 4), 2),
    GenerationC = rep(c(-0.5, 0.5), times = 6),
    GenerationType = c(rep(c("GN", "GN_1"), 3), rep(c("IN", "IN_1"), 3)),
    GenerationTypeGroup = rep(rep(1:3, each = 2), 2),
    GenerationTypeLabel = factor(GenerationType, levels = generation_type_levels, labels = generation_type_labels)
  )
  if(missing(frame)) return(generation_type_map)
  left_join(frame, generation_type_map)
}

