library(ggplot2)
devtools::load_all()

data("Sampled")

Sampled <- Sampled %>%
  dplyr::filter(
    TeamStatus == "V",
    Exp == "100LaborMinutes",
    Strategy == "Diachronic"
  )

ggplot(Sampled) +
  aes(SessionTime, SessionInventorySize, group = Generation) +
  geom_line() +
  facet_wrap("TeamID")

data("TeamPerformance")

TeamPerformance <- TeamPerformance %>%
  dplyr::filter(
    TeamStatus == "V",
    Exp == "100LaborMinutes"
  )

G110Performance <- dplyr::filter(TeamPerformance, TeamID == "G110")
G110Performance

data("PlayerPerformance")

PlayerPerformance <- PlayerPerformance %>%
  dplyr::filter(
    SessionStatus == "V",
    Exp == "100LaborMinutes"
  )

G110Players <- dplyr::filter(PlayerPerformance, TeamID == "G110")
