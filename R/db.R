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
#'
#' Tries to use a local copy if it's available,
#' unless force is set to TRUE.
collect_tbl <- function(name, force = FALSE) {
  local_tbl <- file.path("data-raw/tables/", paste0(name, ".csv"))
  if(file.exists(local_tbl) & !force) {
    frame <- readr::read_csv(local_tbl)

    if("ID_Player" %in% frame) {
      # convert ID_Player from int back to character
      frame$ID_Player <- as.character(frame$ID_Player)
    }

    return(frame)
  } else {
    con <- connect_db()
    frame <- tbl(con, name) %>% collect()
    write.csv(frame, local_tbl, row.names = FALSE)
    RMySQL::dbDisconnect(con)
    return(frame)
  }
}
