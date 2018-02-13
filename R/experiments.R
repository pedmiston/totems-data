filter_from_manifest <- function(manifest_data, frame = NULL) {
  e <- new.env()
  data(list = manifest_data, envir = e)
  valid <- dplyr::filter(e[[manifest_data]], TeamStatus == "valid", SessionStatus == "valid") %>%
    select(PlayerID, TeamID)
  if(is.null(frame)) return(valid)
  dplyr::inner_join(frame, valid)
}

#' Filter valid sessions and teams for Experiment 1.
#' @export
filter_exp1 <- function(frame = NULL) filter_from_manifest("Exp1Manifest", frame)

#' Filter valid sessions and teams for the 50 minute experiment.
#' @export
filter_50min <- function(frame = NULL) filter_from_manifest("Exp2Manifest", frame)

#' Filter valid sessions and teams for the i
#' @export
filter_selfother <- function(frame = NULL) filter_from_manifest("Exp3Manifest", frame)

#' Filter valid sessions and teams for Experiment 4.
#' @export
filter_teamsize <- function(frame = NULL) filter_from_manifest("Exp4Manifest", frame)
