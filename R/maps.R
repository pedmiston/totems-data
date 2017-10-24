create_guesses_map <- function(AllGuesses) {
  select_guesses <- function(guesses_hash_col, guesses_col) {
    guesses_hash <- enquo(guesses_hash_col)
    guesses <- enquo(guesses_col)
    select(AllGuesses, !!guesses_hash, !!guesses)
  }
  select_guesses(PrevSessionGuesses, PrevSessionGuessesHash)
  PlayerGuesses <- select(AllGuesses, PrevPlayerGuesses, PrevPlayerGuessesHash)
  TeamGuesses <- select(AllGuesses, PrevTeamGuesses, PrevTeamGuessesHash)


}
