# Load necessary libraries
library(dplyr)
library(here)

# Load the conflict data
conflict_data <- read.csv(here("data", "original", "conflictdata.csv"), header = TRUE)

# Create the binary armed conflict variable
armed_conflict <- conflict_data %>%
  group_by(ISO, year) %>%
  summarize(armed_conflict = ifelse(sum(best, na.rm = TRUE) > 0, 1, 0), .groups = 'drop') %>%
  mutate(year = year + 1)  # Lag the armed conflict variable by 1 year

# View the resulting dataset
print(armed_conflict)
