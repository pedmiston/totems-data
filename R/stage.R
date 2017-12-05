#' @export
recode_stage <- function(frame) {
  stage_levels <- c("learning", "playing")
  stage_labels <- c("Learning", "Playing")
  map <- data_frame(
    Stage = stage_levels,
    StageC = c(-0.5, 0.5),
    StageLabel = factor(stage_levels, levels = stage_levels, labels = stage_labels)
  )
  if(missing(frame)) return(map)
  left_join(frame, map)
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

#' Label time relative to when learning finished.
#' @export
label_stage_time <- function(IndividualGuesses) {
  IndividualGuesses %>%
    group_by(SessionID) %>%
    mutate(StageTime = SessionTime - max(SessionTime[Stage == "learning"])) %>%
    ungroup()
}
