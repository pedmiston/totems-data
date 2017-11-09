library(ggplot2)

Sampled <- Sampled %>%
  dplyr::filter(
    TeamStatus == "V",
    Exp == "50LaborMinutes"
  ) %>%
  recode_strategy()

ggplot(Sampled) +
  aes(TeamTime, InventorySize,
      group = interaction(Strategy, SessionDuration),
      color = Strategy, linetype = factor(SessionDuration)) +
  geom_line(stat = "summary", fun.y = "mean")

SampledBuggy <- Sampled %>%
  dplyr::filter(Strategy != "Synchronic", SessionDuration == 25)

ggplot(SampledBuggy) +
  aes(TeamTime, InventorySize,
      group = interaction(Strategy, SessionDuration),
      color = Strategy, linetype = factor(SessionDuration)) +
  geom_line(stat = "summary", fun.y = "mean") +
  facet_wrap("Strategy")

ggplot(SampledBuggy) +
  aes(SessionTime, InventorySize,
      group = interaction(Strategy, SessionDuration),
      color = Strategy, linetype = factor(SessionDuration)) +
  geom_line(stat = "summary", fun.y = "mean") +
  facet_grid(Generation ~ Strategy)
