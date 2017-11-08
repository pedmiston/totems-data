#' @export
recode_session_duration <- function(frame) {
  levels <- c(25, 50)
  map <- data_frame(
    SessionDuration = levels,
    SessionDurationLabel = factor(levels)
  )
  if(missing(frame)) return(map)
  left_join(frame, map)
}
