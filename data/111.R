library(tidyverse)

library(here)

here()

rawdat <- read.csv(here("original", "maternalmortality.csv"), header = TRUE)

maternal_mortality_subset <- rawdat %>%
  select(Country.Name, X2000:X2019)

maternal_mortality_long <- maternal_mortality_subset %>%
  pivot_longer(cols = X2000:X2019, 
               names_to = "Year",   
               names_prefix = "X",   
               values_to = "MatMor") 

maternal_mortality_long <- maternal_mortality_long %>%
  mutate(Year = as.numeric(Year))

head(maternal_mortality_long, 20)
tail(maternal_mortality_long, 20)

### 

disaster_data <- read.csv(here("original", "disaster.csv"), header = TRUE)

disaster_filtered <- disaster_data %>%
  filter(Year >= 2000 & Year <= 2019, 
         Disaster.Type %in% c("Earthquake", "Drought"))

disaster_subset <- disaster_filtered %>%
  select(Year, ISO, Disaster.Type)


disaster_with_dummies <- disaster_subset %>%
  mutate(
    drought = ifelse(Disaster.Type == "Drought", 1, 0),
    earthquake = ifelse(Disaster.Type == "Earthquake", 1, 0)
  )

disaster_summary <- disaster_with_dummies %>%
  group_by(Year, ISO) %>%
  summarize(
    drought = max(drought),
    earthquake = max(earthquake),
    .groups = "drop"
  )

print(disaster_summary)

write.csv(disaster_summary, "original/disaster_summary.csv", row.names = FALSE)




