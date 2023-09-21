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
