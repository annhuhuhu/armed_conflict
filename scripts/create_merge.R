covariates <- read.csv(here("data", "original", "covariates.csv"), header = TRUE)

merged_data <- armed_conflict %>%
  inner_join(covariates, by = c("ISO", "year"))  # Join on ISO and year

row_count <- merged_data %>%
  group_by(ISO) %>%
  summarize(row_count = n()) 

print(row_count)

write.csv(merged_data, here("data", "final_analytical_data.csv"), row.names = FALSE)
