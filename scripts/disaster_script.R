#load packages
library(tidyverse)
library("usethis")
library(here)

here()

#read in raw data
rawdata <- read.csv(here("original", "disaster.csv"), header = TRUE)

#use filter function to subset the data set to only include years 2000-2019 and disaster types "Earthquake" and "Drought"
data <- rawdata %>%
  filter(Year >= 2000 & Year <=2019 & Disaster.Type %in% c("Earthquake", "Drought")) %>%
  select(c("Year", "ISO", "Disaster.Type")) %>%
  mutate(Drought = ifelse(Disaster.Type == "Drought",1,0),
         Earthquake = ifelse(Disaster.Type == "Earthquake", 1, 0)) %>%
  group_by(Year, ISO) %>%
  summarize(Drought = max(Drought), Earthquake = max(Earthquake))

#subset data to only the variables Year, ISO, Disaster.type
data <- rawdata %>%
  select(Country.Name, X2000:X2019)

#use pivot_longer to convert data into long format
data <- pivot_longer(data, cols = X2000:X2019, names_to = "year", values_to = "MatMor")

#remove the "X" in front of each column name for the year
data$year <- sub("X", "", data$year)

#make sure year is stored as a numeric
data$year <- as.numeric(data$year)

#check if year is numeric
class(data$year)