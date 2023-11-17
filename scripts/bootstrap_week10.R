#Create a table (in HTML or PDF) that shows the differences in median (with the BCa bootstrap 
#95% confidence intervals) maternal, infant, neonatal, and under-5 mortality between the
#countries exposed to versus not exposed to armed conflict for the year 2017. Be specific about
#the sample sizes used in each statistic. The table should be fully reproducible. Push the
#script that creates the table to your GitHub repository.

#load libraries
library(dplyr)
library(tidyr)
library(here)
library(boot)
library(tidyverse)
library(knitr)

#clear history
rm(list=ls(all=T)) 

data <- read.csv(here("clean", "final_data.csv"), header = TRUE)
data2017 <- data %>%
  filter(year == 2017) %>%
  drop_na(Maternal.Mortality.rate, Infant.mortality.rate, Under.5.mortality.rate, Neonatal.mortality.rate)

# Subset the Data Frame for the year 2017

matmor.arm1 <- data %>%
  filter(year==2017 & !is.na(Maternal.Mortality.rate) & armconf1 ==1) %>%
  select(country_name, Maternal.Mortality.rate)
matmor.arm0 <- data %>%
  filter(year==2017 & !is.na(Maternal.Mortality.rate) & armconf1 ==0) %>%
  select(country_name, Maternal.Mortality.rate)

B <- 1000

med.diff <- rep(NA, B)
for(b in 1:B){
  resample.arm1 <- matmor.arm1[sample(nrow(matmor.arm1), size = nrow(matmor.arm1), replace = TRUE),]
  resample.arm0 <- matmor.arm0[sample(nrow(matmor.arm0), size = nrow(matmor.arm0), replace = TRUE),]
  med.diff[b] <- median(resample.arm1$Maternal.Mortality.rate) - median(resample.arm0$Maternal.Mortality.rate)
}
  
head(resample.arm1, 12)

#make histogram of bootstrap distribution
hist(med.diff, main = "Distribution of boostrap statistic")

#bootstrap for matmor
getmeddiff <- function(data, indices) {
  sample_data <- data[indices, ]
  group_meds <- tapply(sample_data$Maternal.Mortality.rate, sample_data$armconf1, FUN = median)
  med.diff <- group_meds[2] - group_meds[1]
  return(med.diff)
}

bootout_mat <- boot(data2017, statistic = getmeddiff, strata = data2017$armconf1, R=1000)
bootout_mat

#get confidence intervals
boot.ci(boot.out = bootout, conf = 0.95, type = "bca")

#bootstrap statistics for matmor: t1= 126.5, bias= 14.4095, SE= 64.77766, CI= 43.8, 290.3)

#bootstrap for infant mor
getmeddiff <- function(data, indices) {
  sample_data <- data[indices, ]
  group_meds <- tapply(sample_data$Infant.mortality.rate, sample_data$armconf1, FUN = median)
  med.diff <- group_meds[2] - group_meds[1]
  return(med.diff)
}

bootout_infant <- boot(data2017, statistic = getmeddiff, strata = data2017$armconf1, R=1000)
bootout_infant

#get confidence intervals
boot.ci(boot.out = bootout, conf = 0.95, type = "bca")

#bootstrap statistics for infant mor: t1= 19.8, bias= -0.9521, SE= 5.943379, CI= 6.60, 29.16)

#bootstrap for under-5 mor
getmeddiff <- function(data, indices) {
  sample_data <- data[indices, ]
  group_meds <- tapply(sample_data$Under.5.mortality.rate, sample_data$armconf1, FUN = median)
  med.diff <- group_meds[2] - group_meds[1]
  return(med.diff)
}

bootout_under5 <- boot(data2017, statistic = getmeddiff, strata = data2017$armconf1, R=1000)
bootout_under5

#get confidence intervals
boot.ci(boot.out = bootout, conf = 0.95, type = "bca")

#bootstrap statistics for Under mor: t1= 28.9, bias= -3.0935, SE= 9.678971, CI= 8.97, 45.68)

#bootstrap for neomor
getmeddiff <- function(data, indices) {
  sample_data <- data[indices, ]
  group_meds <- tapply(sample_data$Neonatal.mortality.rate, sample_data$armconf1, FUN = median)
  med.diff <- group_meds[2] - group_meds[1]
  return(med.diff)
}

bootout_neo <- boot(data2017, statistic = getmeddiff, strata = data2017$armconf1, R=1000)
bootout_neo

#get confidence intervals
boot.ci(boot.out = bootout, conf = 0.95, type = "bca")

#bootstrap statistics for neomor: t1= 12.4, bias= -0.8052, SE= 3.455128, CI= 5.4, 20.1)

#TABLE
# Create a data frame for each variable
results_matmort <-data.frame(
  Variable = "Maternal Mortality",
  Median_Difference = bootout_mat$t0,
  Bootstrap_Standard_Deviation = sd(bootout_mat$t),
  Bootstrap_CI_Lower = boot.ci(boot.out = bootout_mat, conf = 0.95, type = c("bca"))$bca[4],
  Bootstrap_CI_Upper = boot.ci(boot.out = bootout_mat, conf = 0.95, type = c( "bca"))$bca[5]
) 

data.frame(
  Variable = "Infant Mortality",
  Median_Difference = bootout_infant$t0,
  Bootstrap_Standard_Deviation = sd(bootout_infant$t),
  Bootstrap_CI_Lower = boot.ci(boot.out = bootout_infant, conf = 0.95, type = c("bca"))$bca[4],
  Bootstrap_CI_Upper = boot.ci(boot.out = bootout_infant, conf = 0.95, type = c( "bca"))$bca[5]
)

results_under5 <- data.frame(
  Variable = "Under 5 Mortality",
  Median_Difference = bootout_under5$t0,
  Bootstrap_Standard_Deviation = sd(bootout_under5$t),
  Bootstrap_CI_Lower = boot.ci(boot.out = bootout_under5, conf = 0.95, type = c("bca"))$bca[4],
  Bootstrap_CI_Upper = boot.ci(boot.out = bootout_under5, conf = 0.95, type = c("bca"))$bca[5]
)

results_neo <- data.frame(
  Variable = "Neonatal Mortality",
  Median_Difference = bootout_neo$t0,
  Bootstrap_Standard_Deviation = sd(bootout_neo$t),
  Bootstrap_CI_Lower = boot.ci(boot.out = bootout_neo, conf = 0.95, type = c( "bca"))$bca[4],
  Bootstrap_CI_Upper = boot.ci(boot.out = bootout_neo, conf = 0.95, type = c("bca"))$bca[5]
)

# Combine the results into a single data frame
combined_results <- bind_rows(results_infant, results_under5, results_neo)

# Print the table
kable(combined_results, format = "markdown")
