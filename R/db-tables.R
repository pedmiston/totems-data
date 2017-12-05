#' Open a connection to the Totems MySQL DB.
#'
#' Reads DB info from a file "config.yml"
#' expected in the current directory.
#'
#' @return DBI Connection class
connect_db <- function() {
  stopifnot(file.exists("config.yml"))
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
collect_table <- function(name, con) {
  teardown <- missing(con)
  if(missing(con)) con <- connect_db()
  frame <- collect(tbl(con, name))
  if(teardown) RMySQL::dbDisconnect(con)
  return(frame)
}

#' Save all tables in the db.
save_all_tables <- function(dest = "data-raw/tables/") {
  con <- connect_db()
  dir.create(dest, showWarnings = FALSE)
  tables <- RMySQL::dbListTables(con)
  for(name in tables) {
    frame <- collect_table(name, con)
    out <- file.path(dest, paste0(name, ".csv"))
    write.csv(frame, out, row.names = FALSE)
  }
  RMySQL::dbDisconnect(con)
}

#' Read a local copy of a database table.
read_table <- function(name) {
  src <- paste0("data-raw/tables/", name, ".csv")
  readr::read_csv(src)
}
