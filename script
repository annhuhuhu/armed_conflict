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
