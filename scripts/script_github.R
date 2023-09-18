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

#introduce myself to Git
install.packages("usethis")
library(usethis)

usethis::use_git_config(user.name = "Paijani", user.email = "paijani.sheth@mail.utoronto.ca")

#to confirm, generate a git situation-report, your user name and email should appear under Git config (global) 
usethis::git_sitrep()

#create githuub token
usethis::create_github_token() #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

gitcreds::gitcreds_set()

usethis::use_git()
usethis::use_github()

