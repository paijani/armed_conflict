#load libraries
library(here)
library(tidyr)
library(dplyr)

#load data
final_data <- read.csv(here("clean", "final_data.csv"))

#make a table with y outcome as existence of armed conflict in 2018 for each country (1-if yes, 0-if no)
#group by country
#select them based on whether they had an earthquake between 2010 and 2017 (1) or not (0)
#select them based on whether they had an drought between 2010 and 2017 (1) or not (0)

glm_data <- final_data %>%
  dplyr::select(country_name, ISO, year, armconf1, Drought, Earthquake) %>%
  group_by(country_name) %>%
  mutate(conflict_2018 = as.numeric(any(armconf1 == 1 & year == 2019))) %>%
  mutate(Earthquake_2010_17 = as.numeric(any(Earthquake == 1 & year >= 2010 & year <= 2017))) %>%
  mutate(Drought_2010_17 = as.numeric(any(Drought == 1 & year >= 2010 & year <= 2017))) 

#remove the columns we don't need
glm_data <- glm_data %>%
  select(-armconf1, -Drought, -Earthquake, -year)

#remove all the rows for each country except the first (since they're all the same anyways)
glm_data <- glm_data %>%
  slice(1) %>%
  ungroup() 

#Q1
earthquake_countries <- length(which(glm_data$Earthquake_2010_17 == 1))
earthquake_countries

drought_countries <- length(which(glm_data$Drought_2010_17 == 1))
drought_countries

#Q2
both <- length(which(glm_data$Drought_2010_17 == 1 & glm_data$Earthquake_2010_17 == 1))
both

#22

#Q3
Can <- all(glm_data$country_name == "Canada" & glm_data$Drought_2010_17 == 0 & glm_data$Earthquake_2010_17 == 1)
Can

#Q4
glm <- glm(conflict_2018 ~ Earthquake_2010_17 + Drought_2010_17, family = binomial(link="logit"), data = glm_data)
summary(glm)
exp(coef(glm)) #ORs
coef(glm)
exp(confint(glm))

newdata = data.frame(Earthquake_2010_17 = 0, Drought_2010_17 = 0)
predict(glm, newdata) #default is to calculate the logit
predict(glm, newdata, type="response") #tyoe= response calculates the probabilities
