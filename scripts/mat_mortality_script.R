#load packages
library(dplyr)
library(tidyr)
library(here)

here()

#read in raw data
rawdata <- read.csv(here("original", "maternalmortality.csv"), header = TRUE)

#subset data to have only the variables Country.Name, X2000-X2019
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

write.csv(data, file = "clean_maternal.csv")



