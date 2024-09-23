# Load the covariates data
covariates <- read.csv(here("data", "original", "covariates.csv"), header = TRUE)

# Merge armed conflict and covariates data
merged_data <- armed_conflict %>%
  inner_join(covariates, by = c("ISO", "year"))  # Join on ISO and year

# Check for completeness (20 rows per country)
row_count <- merged_data %>%
  group_by(ISO) %>%
  summarize(row_count = n()) 

# Print countries with row counts
print(row_count)

# Save the final analytical data to a CSV file
write.csv(merged_data, here("data", "final_analytical_data.csv"), row.names = FALSE)
