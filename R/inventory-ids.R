#' Append a new result to an inventory.
#' @export
calculate_final_inventory_id <- function(prev_inventory_id, result) {
  inventory_items <- strsplit(prev_inventory_id, split = "-") %>%
    purrr::map(as.integer) %>%
    unlist()
  if(result %in% inventory_items) return(prev_inventory_id)
  append(inventory_items, result) %>%
    sort() %>%
    paste(collapse = "-")
}
