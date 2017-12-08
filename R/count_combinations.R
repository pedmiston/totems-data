#' Count unique guesses (with replacement).
#'
#' @export
count_unique_guesses_with_replacement <- function(n_items) {
  n_combn_with_replacement <- function(guess_size) {
    nrow(expand.grid(rep(list(1:n_items), guess_size)))
  }
  sum(sapply(1:4, n_combn_with_replacement))
}

#' Count unique guesses (no replacement).
#' @export
count_unique_guesses <- function(n_items) {
  n_combn <- function(guess_size) {
    ncol(combn(1:n_items, guess_size))
  }
  sum(sapply(1:4, n_combn))
}
