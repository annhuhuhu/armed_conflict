library(tidyverse)
library(here)
library(kableExtra)
library(table1)
library(flextable)
library(dplyr)
library(purrr)
library(readr)
library(here)


matmor <- read.csv(here("data", "original", "maternalmortality.csv"), header = TRUE)
matmor <- matmor |>
  dplyr::select(Country.Name, X2000:X2019) |>
  pivot_longer(cols = starts_with("X"),
               names_to = "year",
               names_prefix = "X",
               values_to = "MatMor") |>
  mutate(year = as.numeric(year))
head(matmor, 20)
tail(matmor, 20)

disaster <- read.csv(here("data", "original", "disaster.csv"), header = TRUE)
disaster |>
  dplyr::filter(Year >= 2000 & Year <= 2019) |>
  dplyr::filter(Disaster.Type %in% c("Earthquake", "Drought")) |>
  dplyr::select(Year, ISO, Disaster.Type) |>
  rename(year = Year) |>
  group_by(year, ISO) |>
  mutate(drought0 = ifelse(Disaster.Type == "Drought", 1, 0),
         earthquake0 = ifelse(Disaster.Type == "Earthquake", 1, 0)) |>
  summarize(drought = max(drought0),
            earthquake = max(earthquake0)) |> 
  ungroup() -> disasters 


clean_mortality_data <- function(data, mortality_type) {
  cleaned_data <- data %>%
    select(Country, starts_with("Year")) %>%
    pivot_longer(cols = starts_with("Year"), 
                 names_to = "Year", 
                 values_to = mortality_type) %>%
    mutate(Year = as.numeric(gsub("Year_", "", Year)))  # Adjust year format if necessary
  return(cleaned_data)
}

infant <- read.csv(here("data", "original", "infantmortality.csv"), header = TRUE)

neon <- read.csv(here("data", "original", "neonatalmortality.csv"), header = TRUE)

underfive <- read.csv(here("data", "original", "under5mortality.csv"), header = TRUE)


cleaned_maternal <- clean_mortality_data(matmor)
cleaned_infant <- clean_mortality_data(infant)
cleaned_neon <- clean_mortality_data(neon)
cleaned_underfive <- clean_mortality_data(underfive)


merged_data <- reduce(
  list(cleaned_maternal, cleaned_infant, cleaned_neon, cleaned_underfive), 
  ~ full_join(.x, .y, by = c("Country", "Year"))
)

print(merged_data)

library(countrycode)
yourdata$ISO <- countrycode(yourdata$Country.Name,
                            origin = "country.name",
                            destination = "iso3c")


# Load the disaster data
disaster <- read.csv(here("data", "original", "disaster.csv"), header = TRUE)

# Clean and transform the disaster data
disasters <- disaster |> 
  filter(Year >= 2000 & Year <= 2019) |> 
  filter(Disaster.Type %in% c("Earthquake", "Drought")) |> 
  select(Year, ISO, Disaster.Type) |> 
  rename(year = Year) |> 
  group_by(year, ISO) |> 
  mutate(
    drought0 = ifelse(Disaster.Type == "Drought", 1, 0),
    earthquake0 = ifelse(Disaster.Type == "Earthquake", 1, 0)
  ) |> 
  summarize(
    drought = max(drought0),
    earthquake = max(earthquake0)
  ) |> 
  ungroup() |> 
  select(year, ISO, drought, earthquake)  # Select only the required columns


write.csv(disasters, here("data", "cleaned", "cleaned_disaster_data.csv"), row.names = FALSE)

print(disasters)


                      