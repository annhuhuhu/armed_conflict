library(tidyverse)
library(here)
library(kableExtra)
library(table1)
library(flextable)
library(dplyr)
library(purrr)
library(readr)
library(here)
library(countrycode)


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

matmor0 <- read.csv(here("data", "original", "maternalmortality.csv"), header = TRUE)
infmor0 <- read.csv(here("data", "original", "infantmortality.csv"), header = TRUE)
neomor0 <- read.csv(here("data", "original", "neonatalmortality.csv"), header = TRUE)
un5mor0 <- read.csv(here("data", "original", "under5mortality.csv"), header = TRUE)
wbfun <- function(dataname, varname){
  dataname |>
    dplyr::select(Country.Name, X2000:X2019) |>
    pivot_longer(cols = starts_with("X"),
                 names_to = "year",
                 names_prefix = "X",
                 values_to = varname) |>
    mutate(year = as.numeric(year)) |>
    arrange(Country.Name, year)
}

matmor <- wbfun(dataname = matmor0, varname = "matmor")
infmor <- wbfun(dataname = infmor0, varname = "infmor")
neomor <- wbfun(dataname = neomor0, varname = "neomor")
un5mor <- wbfun(dataname = un5mor0, varname = "un5mor")

wblist <- list(matmor, infmor, neomor, un5mor)

wblist |> reduce(full_join, by = c('Country.Name', 'year')) -> wbdata


wbdata$ISO <- countrycode(wbdata$Country.Name, 
                          origin = "country.name", 
                          destination = "iso3c")
wbdata <- wbdata |>
  dplyr::select(-Country.Name)


