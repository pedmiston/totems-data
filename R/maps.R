#' Create a map of hashes to vectors of guesses.
create_guesses_map <- function(Guesses) {
  unique_guesses <- function(hash_col, guesses_col) {
    hash_col <- enquo(hash_col)
    guesses_col <- enquo(guesses_col)
    Guesses %>%
      select(Hash = !!hash_col, Guesses = !!guesses_col) %>%
      distinct(Hash, .keep_all = TRUE)
  }

  bind_rows(
    unique_guesses(PrevSessionGuessesHash, PrevSessionGuesses),
    unique_guesses(PrevPlayerGuessesHash, PrevPlayerGuesses),
    unique_guesses(PrevTeamGuessesHash, PrevTeamGuesses)
  ) %>%
    distinct(Hash, .keep_all = TRUE)
}

#' Create a map of inventory ids to items.
create_inventory_map <- function(Guesses) {
  unique_inventories <- function(id_col, inventory_col) {
    id_col <- enquo(id_col)
    inventory_col <- enquo(inventory_col)
    Guesses %>%
      select(ID = !!id_col, Inventory = !!inventory_col) %>%
      distinct(ID, .keep_all = TRUE)
  }

  bind_rows(
    unique_inventories(PrevSessionInventoryID, PrevSessionInventory),
    unique_inventories(PrevPlayerInventoryID, PrevPlayerInventory),
    unique_inventories(PrevTeamInventoryID, PrevTeamInventory)
  ) %>%
    distinct(ID, .keep_all = TRUE)
}
