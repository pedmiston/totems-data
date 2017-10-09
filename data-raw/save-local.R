# save-local.R saves local copies of the tables of the db.
library(dplyr)
devtools::load_all()
con <- connect_db()

dst_dir <- "data-raw/tables/"
if (!dir.exists(dst_dir)) dir.create(dst_dir)

save_local <- function(name) {
  tbl(con, name) %>%
    collect() %>%
    write.csv(paste0(dst_dir, name, ".csv"), row.names = FALSE)
}

invisible(sapply(c("Table_Group", "Table_Player", "Table_Workshop"), save_local))
