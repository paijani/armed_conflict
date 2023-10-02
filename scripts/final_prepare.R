#libraries
library(here)
library(tidyr)
library(dplyr)

#edit script from disaster data cleaning script last week such that there is now variables: 
#year, ISO, dummy variable for earthquake and dummy variable for drought

covs <- read.csv(here("original", "covariates.csv"), header = TRUE)

source(here("scripts", "mat_mortality_script.R"))
source(here("scripts", "disaster_script.R"))
source(here("scripts", "clean_function_script.R"))
source(here("scripts", "clean_conflict.R"))

#put all data frames into list
alllist <- list(covs, merged_data, disaster_cleaned, confdata)
lapply(alllist, FUN= summary)

#merge all data frames in list
finaldata <- alllist %>%
  reduce(left_join, by = c('ISO', 'year')) %>% #join all the columns together
  subset(select = -c(OECD2023, country_name))

# Check if you have 20 rows of data for each country 

print(finaldata %>%
        group_by(ISO) %>%
        summarise(Count = n()), n = 186)

# need to fill in NAs with 0's for armconf1, drought, earthquake

finaldata <- finaldata |>
  mutate(armconf1 = replace_na(armconf1, 0),
         Drought = replace_na(Drought, 0),
         Earthquake = replace_na(Earthquake, 0),
         totdeath = replace_na(totdeath, 0))

write.csv(finaldata, file = here("clean", "finaldata.csv"), row.names = FALSE)

dim(finaldata)
names(finaldata)
length(unique(finaldata$ISO))
