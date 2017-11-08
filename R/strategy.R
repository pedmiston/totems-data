#' Recode Strategy
#'
#' @import dplyr
#' @import magrittr
#' @export
recode_strategy <- function(frame) {
  strategies <- c("Synchronic", "Isolated", "Diachronic")
  map <- data_frame(Strategy = strategies,
                    StrategyLabel = factor(strategies, levels = strategies))

  # Add treatment contrasts
  treatment_contrasts <- contr.treatment(strategies, base = 3) %>%
    as.data.frame() %>%
    rename(Diachronic_v_Synchronic = Synchronic, Diachronic_v_Isolated = Isolated) %>%
    mutate(Strategy = row.names(.))
  map %<>% left_join(treatment_contrasts)

  # Add helmert contrasts
  helmert_contrasts <- contr.helmert(3) %>%
    as.data.frame() %>%
    rename(DvS = V1, DSvI = V2) %>%
    mutate(Strategy = c("Diachronic", "Synchronic", "Isolated"))
  map %<>% left_join(helmert_contrasts)

  if (missing(frame)) return(map)
  left_join(frame, map)
}
