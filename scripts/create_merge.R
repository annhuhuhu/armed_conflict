covariates <- read.csv(here("data", "original", "covariates.csv"), header = TRUE)

source(here("scripts", "create_disaster_mortality.R"))
source(here("scripts", "create_conflict.R"))

alllist <- list(confdata, wbdata, disasters)

alllist |> reduce(full_join, by = c('ISO', 'year')) -> finaldata0

finaldata <- covariates |>
  left_join(finaldata0, by = c('ISO', 'year'))


finaldata <- finaldata |>
  mutate(armconf1 = replace_na(armconf1, 0),
         drought = replace_na(drought, 0),
         earthquake = replace_na(earthquake, 0),
         totdeath = replace_na(totdeath, 0))

write.csv(finaldata, file = here("data", "finaldata.csv"), row.names = FALSE)

finaldata <- read.csv(here("data", "finaldata.csv"), header = TRUE)

names(finaldata)

finaldata %>%
  dplyr::filter(country_name == "Canada")

finaldata %>%
  dplyr::filter(country_name == "Ecuador")

