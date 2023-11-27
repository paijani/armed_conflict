#load libraries
library(here)
library(tidyr)
library(dplyr)
library(optimx)

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

#Edit optimx code to include the drought and earthquake predictors

negll <- function(par) {
  
  y <- glm_data$conflict_2018
  x1 <- glm_data$Earthquake_2010_17
  x2 <- glm_data$Drought_2010_17
  
  #1. Calculate xbeta
  xbeta <- par[1] + par[2] * x1 + par[3] * x2
  
  #2. Calculate p
  p <- exp(xbeta) / (1+ exp(xbeta))
  
  #3. Calculate negative log-likelihood
  val <- -sum(y * log(p) + (1-y) * log(1-p))
  
  return(val)
}

opt <- optimx(
  par = c(0,0,0),
  fn = negll,
  control = list(trace = 0, all.methods = TRUE)
)

summary(opt, order = "convcode")

#Extract hessian matrix for BFGS optimization
hessian_m <- attributes(opt)$details["BFGS", "nhatend"][[1]]
fisher_info <- solve(hessian_m)
prop_se <- sqrt(diag(fisher_info))
prop_se

#compare the beta parameter and se estimates from the glm() function to the BFGS

##glm results (from "week_12_quiz" script)
###     (Intercept) Earthquake_2010_17   Drought_2010_17 
### BETA -2.0463         0.9035             0.9523 
###   SE 0.3043          0.3918             0.3746

##BFGS results
###     (Intercept) Earthquake_2010_17   Drought_2010_17 
### BETA -2.0461        0.9036              0.9523
###   SE 0.3043         0.3918              0.3746 