#' Count unique permutations (with replacement).
#'
#' @export
count_unique_permutations <- function(n_items) {
  purrr::map(1:4, perm_with_replacement, n_items = n_items) %>%
    unlist() %>%
    sum()
}

perm_with_replacement <- function(combn_size, n_items) {
  n <- n_items
  r <- combn_size
  n^r
}


#' Count unique combinations of items (with replacement).
#'
#' @export
count_unique_combinations <- function(n_items) {
  purrr::map(1:4, combn_with_replacement, n_items = n_items) %>%
    unlist() %>%
    sum()
}

combn_with_replacement <- function(n_items, combn_size) {
  n <- n_items
  r <- combn_size
  factorial(n + r - 1)/(factorial(r) * factorial(n-1))
}

combn_without_replacement <- function(combn_size, n_items) {
  n <- n_items
  r <- combn_size
  factorial(n)/(factorial(r) * factorial(n-r))
}
