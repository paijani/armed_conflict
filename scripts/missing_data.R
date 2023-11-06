# Load the final data set

library(here)
final_data <- read.csv(here("clean", "final_data.csv"), header = TRUE)

# Load package

library(naniar)
library(VIM)
library(finalfit)
library(texreg)
library(mice)
library(tidyverse)

# Use one of the missing data visualization packages to describe the patterns of missing data 

naniar::vis_miss(final_data)
VIM::aggr(final_data, numbers = TRUE, prop = c(TRUE, FALSE))

# Create linear models for each outcome

final_data$popdens100 <- final_data$popdens / 100

final_data$GDP1000 <- final_data$GDP / 1000

preds <- as.formula(" ~ armconf1 + GDP1000  + OECD + popdens100 + urban + agedep + male_edu + temp + Earthquake + Drought + ISO + as.factor(year)")

matmormod <- lm(update.formula(preds, Maternal.Mortality.rate ~ .), data = final_data)
infantmormod <- lm(update.formula(preds, Infant.mortality.rate ~ .), data = final_data)
neonatmormod <- lm(update.formula(preds, Neonatal.mortality.rate ~ .), data = final_data)
under5mormod <- lm(update.formula(preds, Under.5.mortality.rate ~ .), data = final_data)

keepvars <- list("armconf1" = "Armed Conflict",
                 "GDP1000 = GDP",
                 "podens100" = "Population Density",
                 "urban" = "Urban", 
                 "agedep" = "Age Dependency",
                 "male_edu" = "Male Education",
                 "temp" = "Average Temperature",
                 "Earthquake" = "Earthquake",
                 "Drought" = "Drought")

screenreg(list(matmormod, under5mormod, infantmormod, neonatmormod),
          ci.force = TRUE,
          custom.coef.map = keepvars,
          custom.model.names = c("Maternal mortality", "Under 5 Mortality", "Infant Mortality", "Neonatal Mortality"),
          caption="Results from linear regression models")

# Use the mice package to multiply impute the final data with 𝑚 = 10 imputations

data_imp <- final_data %>%
  mutate(ISO_num = as.numeric(factor(final_data$ISO))) %>%
  select(-c(country_name, ISO, region))

##Use 2l.pan to impute all continuous level-1 variables

# Look at default imputation methods 
mi0 <- mice(data_imp, seed = 1, m = 1, maxit = 0, print = F)
meth <- mi0$method
meth

# 2l.pan level 1 variables 
pred <- mi0$predictorMatrix
pred

pred[c("GDP","popdens","urban","male_edu","temp","Maternal.Mortality.rate",
       "Infant.mortality.rate","Neonatal.mortality.rate","Under.5.mortality.rate"),"ISO_num"] <- -2
pred

meth[c("GDP","popdens","urban","male_edu","temp","Maternal.Mortality.rate",
       "Infant.mortality.rate","Neonatal.mortality.rate","Under.5.mortality.rate")] <- "2l.pan"

# run mice 
start.time <- Sys.time()
mice.multi.out  <- mice(data_imp, seed = 100, m = 10, maxit = 5,
                        method = meth,
                        predictorMatrix = pred, print = F)

plot(mice.multi.out)

# Check imputed values 
complete.data.multi2 <- complete(mice.multi.out, "all")

head(complete.data.multi2$`1`, n=20)

sum(is.na(complete.data.multi2)) # no missing data 


#########################
## Re-Run Linear Model ##
#########################

## fit analysis model and pool results
mat.mor.lm2 <- with(mice.multi.out, 
                    model1 <- lm(Maternal.Mortality.rate ~ ISO_num + as.factor(year) + GDP + popdens + OECD + 
                                    urban + agedep + male_edu + temp + armconf1 + Drought + Earthquake, 
                                  scale.fix = TRUE,
                                  corstr = "ar1"))
summary(pool(mat.mor.lm2))

infant.mor.lm2 <- with(mice.multi.out, 
                       model1 <- lm(Infant.mortality.rate ~ ISO_num + as.factor(year) + GDP + popdens + OECD + 
                                      urban + agedep + male_edu + temp + armconf1 + Drought + Earthquake, 
                                    scale.fix = TRUE,
                                    corstr = "ar1"))
summary(pool(infant.mor.lm2))

neo.mor.lm2 <- with(mice.multi.out, 
                    model1 <- lm(Neonatal.mortality.rate ~ ISO_num + as.factor(year) + GDP + popdens + OECD + 
                                    urban + agedep + male_edu + temp + armconf1 + Drought + Earthquake, 
                                  scale.fix = TRUE,
                                  corstr = "ar1"))
summary(pool(neo.mor.lm2))

under5.mor.lm2 <- with(mice.multi.out, 
                       model1 <- lm(Under.5.mortality.rate ~ ISO_num + as.factor(year) + GDP + popdens + OECD + 
                                       urban + agedep + male_edu + temp + armconf1 + Drought + Earthquake, 
                                     scale.fix = TRUE,
                                     corstr = "ar1"))
summary(pool(under5.mor.lm2))