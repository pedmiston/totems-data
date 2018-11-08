filter_from_manifest <- function(manifest_data, frame = NULL) {
  e <- new.env()
  data(list = manifest_data, envir = e)
  valid <- dplyr::filter(e[[manifest_data]], TeamStatus == "valid", SessionStatus == "valid") %>%
    select(PlayerID) %>%
    unique()
  if(is.null(frame)) return(valid)
  dplyr::inner_join(frame, valid)
}

#' Filter valid sessions and teams for Experiment 1.
#' @export
filter_exp1 <- function(frame = NULL) filter_from_manifest("Exp1Manifest", frame)

#' Filter valid sessions and teams for the 50 minute experiment.
#' @export
filter_50min <- function(frame = NULL) filter_from_manifest("Manifest50min", frame)

#' Filter valid sessions and teams for the self-other experiment.
#' @export
filter_selfother <- function(frame = NULL) filter_from_manifest("ManifestSelfOther", frame)

#' Filter valid sessions and teams for the scalability experiment.
#' @export
filter_teamsize <- function(frame = NULL) filter_from_manifest("ManifestScalability", frame)

#' Filter valid sessions and teams for the inherited instructions experiment.
#' @export
filter_instructions <- function(frame = NULL) filter_from_manifest("ManifestInstructions", frame)
