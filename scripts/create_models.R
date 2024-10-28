# Load necessary libraries
library(dplyr)
library(plm)
library(stargazer)
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

# Load the dataset
final_data <- read.csv(here("data", "finaldata.csv"), header = TRUE)

# Log-transform GDP and add it to the data
final_data <- final_data %>%
  mutate(log_gdp1000 = log(gdp1000 + 1)) # Added 1 to avoid log(0) issues

# Define the formula with predictors
preds <- as.formula(" ~ armconf1 + log_gdp1000 + OECD + popdens + urban + agedep + male_edu + temp + rainfall1000 + earthquake + drought + as.factor(year)")

# Fit the models
matmor_model <- plm(update(preds, matmor ~ .), data = final_data, index = c("ISO", "year"), model = "within")
infmor_model <- plm(update(preds, infmor ~ .), data = final_data, index = c("ISO", "year"), model = "within")
neomor_model <- plm(update(preds, neomor ~ .), data = final_data, index = c("ISO", "year"), model = "within")
un5mor_model <- plm(update(preds, un5mor ~ .), data = final_data, index = c("ISO", "year"), model = "within")

# Create a summary table
stargazer(matmor_model, infmor_model, neomor_model, un5mor_model, type = "html", out = "mortality_models_summary.html")

