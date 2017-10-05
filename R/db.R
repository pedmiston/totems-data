#' Open a connection to the Totems MySQL DB.
#'
#' Reads DB info from a file "config.yml"
#' expected in the current directory.
#'
#' @return DBI Connection class
#' @export
connect_db <- function() {
  config <- yaml::yaml.load_file("config.yml")
  DBI::dbConnect(RMySQL::MySQL(),
                 dbname = "Totems",
                 host = config$host,
                 port = config$port,
                 user = config$user,
                 password = config$password)
}

#' Create a blank config file "config.yml" in the current directory.
#' @import magrittr
#' @export
create_blank_config <- function() {
  list(host="", port=0, user="", password="") %>%
    yaml::as.yaml() %>%
    writeLines(con = "config.yml")
}

#' Collect a single table from the db.
#' @export
collect_tbl <- function(name) {
  con <- connect_db()
  frame <- tbl(con, name) %>% collect()
  RMySQL::dbDisconnect(con)
  frame
}
