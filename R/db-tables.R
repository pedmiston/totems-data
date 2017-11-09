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
create_blank_config <- function() {
  list(host="", port=0, user="", password="") %>%
    yaml::as.yaml() %>%
    writeLines(con = "config.yml")
}

#' Collect a single table from the db.
collect_table <- function(name, con, save = TRUE) {
  teardown <- missing(con)
  if(missing(con)) {
    con <- connect_db()
  }
  frame <- collect(tbl(con, name))
  if(save) {
    out <- paste0("data-raw/tables/", name, ".csv")
    write.csv(frame, out, row.names = FALSE)
  }
  if(teardown) RMySQL::dbDisconnect(con)
  return(frame)
}

#' Collect all tables in the db.
collect_tables <- function() {
  con <- connect_db()
  tables <- RMySQL::dbListTables(con)
  for(table in tables) {
    collect_table(table, con)
  }
  RMySQL::dbDisconnect(con)
}

#' Read a local version of the table.
read_table <- function(name) {
  src <- paste0("data-raw/tables/", name, ".csv")
  readr::read_csv(src)
}
