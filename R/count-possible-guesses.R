#' Count unique guesses (with replacement).
#'
#' @export
count_unique_guesses <- function(n_items) {
  purrr::map(1:4, perm_with_replacement, n_items = n_items) %>%
    unlist() %>%
    sum()
}

perm_with_replacement <- function(combn_size, n_items) {
  n <- n_items
  r <- combn_size
  n^r
}

#' Count unique combinations of items.
#'
#' @export
count_unique_combinations <- function(n_items) {
  purrr::map(1:4, function(guess_size) {
    if(guess_size <= n_items) {
      combn_without_replacement(n_items, guess_size)
    }
  }) %>%
    unlist() %>%
    sum()
}

combn_without_replacement <- function(n_items, combn_size) {
  n <- n_items
  r <- combn_size
  factorial(n)/(factorial(r) * factorial(n-r))
}
