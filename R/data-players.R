#' Get player data from the database.
#' @import dplyr
data_players <- function(con) {
  Players <- tbl(con, "Table_Player")
  collect(Players)
}
