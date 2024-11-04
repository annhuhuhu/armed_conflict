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
library(naniar)
library(mice) 
library(broom)   
library(knitr)

# 
finaldata <- read.csv(here("data", "finaldata.csv"), header = TRUE)

finaldata <- finaldata %>%
  mutate(loggdp = log(gdp1000 + 1))


finaldata <- finaldata %>%
  mutate(ISOnum = as.numeric(as.factor(ISO)))

midata <- finaldata %>%
  select(-country_name, -ISO, -region, -year)

# Initial dry run to get method and predictor settings
mice0 <- mice(midata, seed = 100, m = 5, maxit = 0)

# Define methods for imputation
meth <- mice0$method
meth[c("urban", "male_edu", "temp", "rainfall1000", "matmor", "infmor", "neomor", "un5mor", "loggdp", "popdens")] <- "2l.lmer"

# Define predictor matrix
pred <- mice0$predictorMatrix
pred[c("urban", "male_edu", "temp", "rainfall1000", "matmor", "infmor", "neomor", "un5mor", "loggdp", "popdens"), "ISOnum"] <- -2
  
# Perform MI
mice.multi.out <- mice(midata, seed = 100, m = 10, maxit = 20, method = meth, predictorMatrix = pred)

#Check for convergence
plot(mice.multi.out)

# Define the formula for the models
preds <- as.formula(" ~ armconf1 + loggdp + popdens + urban + agedep + male_edu + temp + rainfall1000 + earthquake + drought")

# Fit models using with()
matmormod <- with(mice.multi.out, lm(update.formula(preds, matmor ~ .), data = finaldata))
un5mormod <- with(mice.multi.out, lm(update.formula(preds, un5mor ~ .), data = finaldata))
infmormod <- with(mice.multi.out, lm(update.formula(preds, infmor ~ .), data = finaldata))
neomormod <- with(mice.multi.out, lm(update.formula(preds, neomor ~ .), data = finaldata)) 

# Pool the results
matmor_pool <- pool(matmormod)
un5mor_pool <- pool(un5mormod)
infmor_pool <- pool(infmormod)
neomor_pool <- pool(neomormod)

# Summary of pooled results
summary(matmor_pool)
summary(un5mor_pool)
summary(infmor_pool)
summary(neomor_pool)

# Complete case analysis
complete_data <- na.omit(finaldata)

cc_matmor <- lm(update.formula(preds, matmor ~ .), data = complete_data)
cc_un5mor <- lm(update.formula(preds, un5mor ~ .), data = complete_data)
cc_infmor <- lm(update.formula(preds, infmor ~ .), data = complete_data)
cc_neomor <- lm(update.formula(preds, neomor ~ .), data = complete_data)

# Summary of complete case results
summary(cc_matmor)
summary(cc_un5mor)
summary(cc_infmor)
summary(cc_neomor)

# Tidy the results
tidy_matmor <- tidy(matmor_pool)
tidy_un5mor <- tidy(un5mor_pool)
tidy_infmor <- tidy(infmor_pool)
tidy_neomor <- tidy(neomor_pool)

tidy_cc_matmor <- tidy(cc_matmor)
tidy_cc_un5mor <- tidy(cc_un5mor)
tidy_cc_infmor <- tidy(cc_infmor)
tidy_cc_neomor <- tidy(cc_neomor)

# Combine results for comparison
comparison_table <- data.frame(
  Variable = tidy_matmor$term,
  MI_Estimate = tidy_matmor$estimate,
  CC_Estimate = tidy_cc_matmor$estimate
  # Add other models similarly
)

# View the comparison table
print(comparison_table)


