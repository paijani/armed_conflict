#load packages
library(dplyr)
library(tidyr)
library(here)

#QUESTION 1
#read in raw data
data <- read.csv(here("clean", "final_data.csv"), header = TRUE)

# Subset the DataFrame for the year 2017
data1 <- data %>%
  filter(year == 2017) %>%
  group_by(country_name) %>%
  summarise(missing_count = sum(is.na(Maternal.Mortality.rate)))

count <- sum(data1$missing_count == 1)
count

#QUESTION 2
data2 <- data %>%
  filter(year == 2017) %>%
  filter(!country_name %in% c('Andorra', 'Dominica', 'Marshall Islands'))

count_armconf_1 <- data2 %>%
  filter(armconf1 ==1) %>%
  summarise(count_1 = n_distinct(country_name))

count_armconf_1

count_armconf_0 <- data2 %>%
  filter(armconf1 ==0) %>%
  summarise(count_0 = n_distinct(country_name))

count_armconf_0

#QUESTION 3
data3 <- data %>%
  filter(year == 2017) %>%
  filter(!country_name %in% c('Andorra', 'Dominica', 'Marshall Islands')) %>%
  filter(armconf1 == 1)

median(data3$Maternal.Mortality.rate)

#sd of median of bootstrap distribution is SE