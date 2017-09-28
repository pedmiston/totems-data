library(devtools)
library(tidyverse)

ItemLabels <- read_csv("data-raw/answer-key.csv") %>%
  select(Number, Name) %>%
  unique()

StartingItems <- data_frame(
  Number = 1:6,
  Name = c("Big_Tree.jpg", "Tree.jpg", "Stone.jpg", "Red_Berry.jpg", "Blue_Berry.jpg", "Antler.jpg")
)

ItemLabels <- bind_rows(StartingItems, ItemLabels)

use_data(ItemLabels, overwrite = TRUE)
