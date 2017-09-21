#' @export
guess_generation <- function(totems_data, time_col) {
  # Set generation for everyone then change for Diachronic teams
  totems_data$Generation <- 1
  diachronic_generation_duration <- 25 * 60  # minutes * seconds
  is_diachronic <- totems_data$Strategy == "Diachronic"
  is_first_gen <- totems_data[is_diachronic, time_col] <= diachronic_generation_duration
  totems_data[is_diachronic, "Generation"] <- ifelse(is_first_gen, 1, 2)
  totems_data
}
