#edit script from disaster data cleaning script last week such that there is now variables: 
#year, iso, dummy variable for earthquake and dummy vairable for drought

covs <- read.csv(here("original", "covariates.csv"), header = TRUE)

source(here("scripts", "mat_mortality_script.R"))
source(here("scripts", "disaster_script.R"))
source(here("scripts", "clean_function_script.R"))


#put all data frames into list
alllist <- list(covs, confdata, wbdata, disasters)

#merge all data frames in list
alllist |> reduce(left_join, by = c('ISO', 'year')) -> finaldata

# need to fill in NAs with 0's for armconf1, drought, earthquake
finaldata <- finaldata |>
  mutate(armconf1 = replace_na(armconf1, 0),
         drought = replace_na(drought, 0),
         earthquake = replace_na(earthquake, 0),
         totdeath = replace_na(totdeath, 0))

write.csv(finaldata, file = here("data", "finaldata.csv"), row.names = FALSE)

dim(finaldata)
names(finaldata)
length(unique(finaldata$ISO))