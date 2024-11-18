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
library(texreg)
library(boot)

finaldata <- read.csv(here("data", "finaldata.csv"), header = TRUE)

data2017matmor <- finaldata |>
  dplyr::filter(year == 2017) |>
  dplyr::filter(!is.na(matmor))

data2017infmor <- finaldata |>
  dplyr::filter(year == 2017) |>
  dplyr::filter(!is.na(infmor))

data2017neomor <- finaldata |>
  dplyr::filter(year == 2017) |>
  dplyr::filter(!is.na(neomor))

data2017un5mor <- finaldata |>
  dplyr::filter(year == 2017) |>
  dplyr::filter(!is.na(un5mor))

data2017matmor |>
  group_by(armconf1) |>
  summarise(n = n(),
            median.matmor = median(matmor, na.rm = T))

data2017infmor |>
  group_by(armconf1) |>
  summarise(n = n(),
            median.infmor = median(infmor, na.rm = T))

data2017neomor |>
  group_by(armconf1) |>
  summarise(n = n(),
            median.neomor = median(neomor, na.rm = T))

data2017un5mor |>
  group_by(armconf1) |>
  summarise(n = n(),
            median.un5mor = median(un5mor, na.rm = T))

obs.med.diff.matmor <- median(data2017matmor[data2017matmor$armconf1 == 1,]$matmor) -
  median(data2017matmor[data2017matmor$armconf1 == 0,]$matmor)
obs.med.diff.matmor

obs.med.diff.infmor <- median(data2017infmor[data2017infmor$armconf1 == 1,]$infmor) -
  median(data2017infmor[data2017infmor$armconf1 == 0,]$infmor)
obs.med.diff.infmor

obs.med.diff.neomor <- median(data2017neomor[data2017neomor$armconf1 == 1,]$neomor) -
  median(data2017neomor[data2017neomor$armconf1 == 0,]$neomor)
obs.med.diff.neomor

obs.med.diff.un5mor <- median(data2017un5mor[data2017un5mor$armconf1 == 1,]$un5mor) -
  median(data2017un5mor[data2017un5mor$armconf1 == 0,]$un5mor)
obs.med.diff.un5mor

matmor.arm1 <- finaldata |>
  dplyr::filter(year == 2017 & !is.na(matmor) & armconf1 == 1) |>
  dplyr::select(ISO, matmor)
matmor.arm0 <- finaldata |>
  dplyr::filter(year == 2017 & !is.na(matmor) & armconf1 == 0) |>
  dplyr::select(ISO, matmor)

infmor1 <- finaldata |>
  dplyr::filter(year == 2017 & !is.na(infmor) & armconf1 == 1) |>
  dplyr::select(ISO, infmor)
infmor0 <- finaldata |>
  dplyr::filter(year == 2017 & !is.na(infmor) & armconf1 == 0) |>
  dplyr::select(ISO, infmor)

neomor1 <- finaldata |>
  dplyr::filter(year == 2017 & !is.na(neomor) & armconf1 == 1) |>
  dplyr::select(ISO, neomor)
neomor0 <- finaldata |>
  dplyr::filter(year == 2017 & !is.na(neomor) & armconf1 == 0) |>
  dplyr::select(ISO, neomor)

un5mor1 <- finaldata |>
  dplyr::filter(year == 2017 & !is.na(un5mor) & armconf1 == 1) |>
  dplyr::select(ISO, un5mor)
un5mor0 <- finaldata |>
  dplyr::filter(year == 2017 & !is.na(un5mor) & armconf1 == 0) |>
  dplyr::select(ISO, un5mor)

set.seed(2024)
B <- 1000

med.diff <- rep(NA, B)
for(b in 1:B){
  resamp.arm1 <- matmor.arm1[sample(nrow(matmor.arm1), size = nrow(matmor.arm1), replace = TRUE),]
  resamp.arm0 <- matmor.arm0[sample(nrow(matmor.arm0), size = nrow(matmor.arm0), replace = TRUE),]
  med.diff[b] <- median(resamp.arm1$matmor) - median(resamp.arm0$matmor)
}
head(resamp.arm1, 12)

med.diffin <- rep(NA, B)
for(b in 1:B){
  reinfmor1 <- infmor1[sample(nrow(infmor1), size = nrow(infmor1), replace = TRUE),]
  reinfmor0 <- infmor0[sample(nrow(infmor0), size = nrow(infmor0), replace = TRUE),]
  med.diff[b] <- median(reinfmor1$infmor) - median(reinfmor0$infmor)
}
head(reinfmor1, 12)
hist(med.diff, main = "Distribution of bootstrap statistic")

med.diffneomor <- rep(NA, B)
for(b in 1:B){
  reneomor1 <- neomor1[sample(nrow(neomor1), size = nrow(neomor1), replace = TRUE),]
  reneomor0 <- neomor0[sample(nrow(neomor0), size = nrow(neomor0), replace = TRUE),]
  med.diff[b] <- median(reneomor1$neomor) - median(reneomor0$neomor)
}
head(reneomor1, 12)
hist(med.diff, main = "Distribution of bootstrap statistic")

med.diffun5mor <- rep(NA, B)
for(b in 1:B){
  reun5mor1 <- un5mor1[sample(nrow(un5mor1), size = nrow(un5mor1), replace = TRUE),]
  reun5mor0 <- un5mor0[sample(nrow(un5mor0), size = nrow(un5mor0), replace = TRUE),]
  med.diff[b] <- median(reun5mor1$un5mor) - median(reun5mor0$un5mor)
}
head(reun5mor1, 12)
hist(med.diff, main = "Distribution of bootstrap statistic")


###
getmeddiff <- function(data, indices) {
  sample_data <- data[indices, ]
  group_meds <- tapply(sample_data$matmor, sample_data$armconf1, FUN = function(x) median(x,na.rm=TRUE))
  meddiff <- group_meds[2] - group_meds[1]
  return(meddiff)
}

bootout <- boot(data2017, statistic = getmeddiff, strata = data2017$armconf1, R = 1000)
bootout

### 
getmeddiffin <- function(data, indices) {
  sample_data <- data[indices, ]
  group_meds <- tapply(sample_data$infmor, sample_data$armconf1, FUN = function(x) median(x,na.rm=TRUE))
  meddiff <- group_meds[2] - group_meds[1]
  return(meddiff)
}

bootoutin <- boot(data2017infmor, statistic = getmeddiffin, strata = data2017infmor$armconf1, R = 1000)
bootoutin

bootoutin$t0

head(bootoutin$t)

sd(bootoutin$t)

quantile(bootoutin$t, probs = c(0.025, 0.975))

### 
getmeddiffne <- function(data, indices) {
  sample_data <- data[indices, ]
  group_meds <- tapply(sample_data$neomor, sample_data$armconf1, FUN = function(x) median(x,na.rm=TRUE))
  meddiff <- group_meds[2] - group_meds[1]
  return(meddiff)
}

bootoutne <- boot(data2017neomor, statistic = getmeddiffin, strata = data2017neomor$armconf1, R = 1000)
bootoutne

bootoutne$t0

head(bootoutne$t)

sd(bootoutne$t)

quantile(bootoutne$t, probs = c(0.025, 0.975))

### 
getmeddiff5 <- function(data, indices) {
  sample_data <- data[indices, ]
  group_meds <- tapply(sample_data$un5mor, sample_data$armconf1, FUN = function(x) median(x,na.rm=TRUE))
  meddiff <- group_meds[2] - group_meds[1]
  return(meddiff)
}

bootout5 <- boot(data2017un5mor, statistic = getmeddiffin, strata = data2017un5mor$armconf1, R = 1000)
bootout5

bootout5$t0

head(bootout5$t)

sd(bootout5$t)

quantile(bootout5$t, probs = c(0.025, 0.975))


### 
2 * bootout$t0 - quantile(bootout$t, probs = 0.975)
2 * bootout$t0 - quantile(bootout$t, probs = 0.025)
boot.ci(boot.out = bootout, conf = 0.95, type = c("basic", "perc", "bca"))

### 
2 * bootoutin$t0 - quantile(bootoutin$t, probs = 0.975)
2 * bootoutin$t0 - quantile(bootoutin$t, probs = 0.025)
boot.ci(boot.out = bootoutin, conf = 0.95, type = c("basic", "perc", "bca"))

### 
2 * bootoutne$t0 - quantile(bootoutne$t, probs = 0.975)
2 * bootoutne$t0 - quantile(bootoutne$t, probs = 0.025)
boot.ci(boot.out = bootoutne, conf = 0.95, type = c("basic", "perc", "bca"))

### 
2 * bootout5$t0 - quantile(bootout5$t, probs = 0.975)
2 * bootout5$t0 - quantile(bootout5$t, probs = 0.025)
boot.ci(boot.out = bootout5, conf = 0.95, type = c("basic", "perc", "bca"))

# Create a data frame with the results
results <- data.frame(
  Mortality = c("Maternal", "Infant", "Neonatal", "Under-5"),
  Lower = c(
    2 * bootout$t0 - quantile(bootout$t, probs = 0.975),
    2 * bootoutin$t0 - quantile(bootoutin$t, probs = 0.975),
    2 * bootoutne$t0 - quantile(bootoutne$t, probs = 0.975),
    2 * bootout5$t0 - quantile(bootout5$t, probs = 0.975)
  ),
  Upper = c(
    2 * bootout$t0 - quantile(bootout$t, probs = 0.025),
    2 * bootoutin$t0 - quantile(bootoutin$t, probs = 0.025),
    2 * bootoutne$t0 - quantile(bootoutne$t, probs = 0.025),
    2 * bootout5$t0 - quantile(bootout5$t, probs = 0.025)
  )
)

# Function to extract CI values
extract_ci <- function(boot_obj) {
  ci <- boot.ci(boot.out = boot_obj, conf = 0.95, type = c("basic", "perc", "bca"))
  data.frame(
    Basic = paste0("(", round(ci$basic[4], 2), ", ", round(ci$basic[5], 2), ")"),
    Percentile = paste0("(", round(ci$percent[4], 2), ", ", round(ci$percent[5], 2), ")"),
    BCa = paste0("(", round(ci$bca[4], 2), ", ", round(ci$bca[5], 2), ")")
  )
}

# Extract CI values for each mortality type
ci_values <- bind_rows(
  extract_ci(bootout),
  extract_ci(bootoutin),
  extract_ci(bootoutne),
  extract_ci(bootout5)
)

# Combine results
final_table <- bind_cols(results, ci_values)

# Print the table
print(final_table)

### Infant Mortality: The 95% bootstrap confidence intervals suggest that countries experiencing armed conflict have a median infant mortality rate that is between 9.56 and 28.44 deaths per 1,000 live births higher than countries not experiencing armed conflict (using the percentile method). This indicates a significant and substantial increase in infant mortality associated with armed conflict.
### Under-5 Mortality: For under-5 mortality, the confidence intervals indicate an even larger difference. Countries with armed conflict have a median under-5 mortality rate that is between 14.20 and 41.80 deaths per 1,000 live births higher than countries without armed conflict. This suggests that the impact of armed conflict on child mortality extends beyond infancy and has an even greater effect on children under 5 years old.
### Neonatal Mortality: The confidence intervals for neonatal mortality show a smaller but still significant difference. Countries with armed conflict have a median neonatal mortality rate that is between 4.20 and 12.60 deaths per 1,000 live births higher than countries without conflict. While the impact is less pronounced than for infant and under-5 mortality, it still represents a substantial increase in mortality risk for newborns in conflict-affected areas.





