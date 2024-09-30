library(dplyr)
library(here)

conflict_data <- read.csv(here("data", "original", "conflictdata.csv"), header = TRUE)

conflict_data %>%
  group_by(ISO, year) |>
  summarise(totdeath = sum(best)) |>
  mutate(armconf1 = ifelse(totdeath < 25, 0, 1)) |>
  ungroup() |>
  mutate(year = year + 1) -> confdata

table(confdata$armconf1)
head(confdata)