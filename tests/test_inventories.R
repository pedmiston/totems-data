library(ggplot2)
data("Guesses")

DG2 <- Guesses %>%
  dplyr::filter(Exp == "50LaborMinutes", TeamStatus == "V", Strategy == "Diachronic", Generation == 2) %>%
  mutate(
    PrevSessionInventoryIDNChar = nchar(PrevSessionInventoryID),
    PrevTeamInventoryIDNChar = nchar(PrevTeamInventoryID)
  ) %>%
  select(TeamID, SessionTime, PrevSessionInventoryIDNChar, PrevTeamInventoryIDNChar) %>%
  tidyr::gather(InventoryType, InventoryIDNChar, -(TeamID:SessionTime)) %>%
  mutate(InventoryTypeOrdered = factor(InventoryType, levels = c("PrevTeamInventoryIDNChar", "PrevSessionInventoryIDNChar")))

ggDG2 <- ggplot(DG2) +
  aes(SessionTime, InventoryIDNChar, color = InventoryTypeOrdered) +
  geom_line(aes(group = interaction(TeamID, InventoryTypeOrdered))) +
  facet_wrap("TeamID")
ggDG2
ggDG2 %+% dplyr::filter(DG2, TeamID == unique(TeamID)[[1]])
ggDG2 %+% dplyr::filter(DG2, SessionTime < 250, TeamID == unique(TeamID)[[1]])
ggDG2 %+% dplyr::filter(DG2, SessionTime < 25, TeamID == unique(TeamID)[[1]])
ggDG2 %+% dplyr::filter(DG2, TeamID == unique(TeamID)[[1]],
                        InventoryType == "PrevSessionInventoryIDNChar")
ggDG2 %+% dplyr::filter(DG2, TeamID == unique(TeamID)[[1]],
                        InventoryType == "PrevTeamInventoryIDNChar")
