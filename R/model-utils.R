#' Get the predictions from a linear model.
#' @export
get_lm_mod_preds <- function(lm_mod) {
  mod_data <- lm_mod$model
  params <- attr(lm_mod$terms, "term.labels")
  x_preds <- unique(mod_data[, params, drop = FALSE])
  cbind(x_preds, predict(lm_mod, x_preds, se = TRUE))
}
