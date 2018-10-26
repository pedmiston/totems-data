#' Transform SessionTime for polynomial models.
#' @export
recode_session_time <- function(frame) {
  session_time_map <- data_frame(
    SessionTime = 0:50,
    SessionTimeC = -25:25,
    SessionTimeZ = (SessionTimeC - mean(SessionTimeC))/sd(SessionTimeC),
    SessionTimeZ_2 = SessionTimeZ^2,
    SessionTimeZ_3 = SessionTimeZ^3
  )
  if(missing(frame)) return(session_time_map)
  left_join(frame, session_time_map)
}
