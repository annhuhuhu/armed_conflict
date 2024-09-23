library(dplyr)
library(here)

conflict_data <- read.csv(here("data", "original", "conflictdata.csv"), header = TRUE)

armed_conflict <- conflict_data %>%
  group_by(ISO, year) %>%
  summarize(armed_conflict = ifelse(sum(best, na.rm = TRUE) >= 25, 1, 0), .groups = 'drop') %>%
  mutate(year = year + 1)  # Lag the armed conflict variable by 1 year

print(armed_conflict)