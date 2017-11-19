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
