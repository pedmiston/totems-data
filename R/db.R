#' Open a connection to the Totems MySQL DB.
#'
#' Reads DB info from a file "config.yml"
#' expected in the current directory.
#'
#' @return DBI Connection class
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
create_blank_config <- function() {
  list(host="", port=0, user="", password="") %>%
    yaml::as.yaml() %>%
    writeLines(con = "config.yml")
}

#' Collect a single table from the db.
collect_tbl <- function(name) {
  con <- connect_db()
  frame <- tbl(con, name) %>% collect()
  RMySQL::dbDisconnect(con)
  frame
}

collect_local <- function(name) {
  local_src <- "data-raw/tables/"
  read.csv(paste0(local_src, name, ".csv"))
}
