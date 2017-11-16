#' Label the stage of the guesser's inventory relative to the team inventory.
label_stage <- function(Guesses) {
  Guesses %>%
    arrange(SessionTime) %>%
    group_by(SessionID) %>%
    mutate(Stage = ifelse(nchar(PrevTeamInventoryID) > nchar(PrevSessionInventoryID),
                          "learning", "playing")) %>%
    ungroup()
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
