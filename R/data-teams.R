#' Get team data from the database.
#' @import dplyr
data_teams <- function(con) {
  Teams <- tbl(con, "Table_Group")
  collect(Teams)
}
