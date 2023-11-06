#write an R script that creates a Table 1 using your favorite package

#clear history 

rm(list=ls(all=T)) 

#load packages
library(table1)
library(here)
library(tidyr)
library(boot)
library(fastmap)

#load final data that table 1 will be created from
final_data <- read.csv(here("clean", "final_data.csv"))

#subset data to show for year 2000
data_2000 <- subset(final_data, year=2000)

#convert the 0's and 1's in the binary variables to yes's and no's
data_2000$armconf1 <- factor(data_2000$armconf1, levels = c(0,1), labels = c("No armed conflict", "Armed conflict"))
data_2000$Drought <- factor(data_2000$Drought, levels = c(0,1), labels = c("No", "Yes"))
data_2000$ Earthquake <- factor(data_2000$Earthquake, levels = c(0,1), labels = c("No", "Yes"))
data_2000$OECD <- factor(data_2000$OECD, levels = c(0,1), labels = c("No", "Yes"))

#label each variable
label(data_2000$GDP) <- "GDP per capita"
label(data_2000$OECD) <- "OECD member"
label(data_2000$popdens) <- "Population Density"
label(data_2000$urban) <- "Urban Residence"
label(data_2000$agedep) <- "Age Dependency Ratio"
label(data_2000$male_edu) <- "Male education"
label(data_2000$temp) <- "Mean Annual Temperature"
label(data_2000$Infant.mortality.rate) <- "Infant Mortality"
label(data_2000$Neonatal.mortality.rate) <- "Neonatal Mortality"
label(data_2000$Under.5.mortality.rate) <- "Under 5 Mortality"
label(data_2000$Maternal.Mortality.rate) <- "Maternal Mortality"
label(data_2000$Drought) <- "Drought"
label(data_2000$Earthquake) <- "Earthquake"
label(data_2000$totdeath) <- "Total number of Deaths"
label(data_2000$armconf1) <- "Armed conflict"

#give units to GDP
units(data_2000$GDP) <- "USD"

table1 <- table1(~ GDP + OECD + popdens + urban + agedep + male_edu + temp + Infant.mortality.rate + Neonatal.mortality.rate + Under.5.mortality.rate + 
         Maternal.Mortality.rate + Drought + Earthquake + totdeath | armconf1, data = data_2000, render.continuous = c(.="Median [Min, Max]"),
       overall=c(left="Total"))

table1
