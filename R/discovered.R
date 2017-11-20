#' @export
recode_discovered <- function(frame) {
  levels <- c(TRUE, FALSE)
  types <- c("discovered", "undiscovered")
  labels <- c("Discovered", "Undiscovered")
  long_labels <- c("Item discovered", "Item never discovered")
  map <- data_frame(
    Discovered = levels,
    DiscoveredType = types,
    DiscoveredC = c(0.5, -0.5),
    DiscoveredLabel = factor(levels, levels = levels, labels = labels),
    DiscoveredLong = factor(levels, levels = levels, labels = long_labels),
    DiscoveredRev = factor(levels, levels = rev(levels), labels = rev(labels))
  )
  if(missing(frame)) return(map)
  left_join(frame, map)
}
