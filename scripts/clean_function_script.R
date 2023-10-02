library(dplyr)
library(tidyr)
library(here)
library(countrycode)
library(purrr)


here()

#read in raw data

clean_infant <- read.csv(here("original", "infantmortality.csv"), header = TRUE)
clean_under5 <- read.csv(here("original", "under5mortality.csv"), header = TRUE)
clean_matmor <- read.csv(here("original", "maternalmortality.csv"), header = TRUE)
clean_neonatal <- read.csv(here("original", "neonatalmortality.csv"), header = TRUE)


clean_data <- function(x, varname) {
  
  #subset data to have only the variables Country.Name, X2000-X2019
  data <- x %>%
    select(Country.Name, X2000:X2019)
  
  #use pivot_longer to convert data into long format
  data <- pivot_longer(data, 
                       cols = X2000:X2019, 
                       names_to = "year", 
                       names_prefix = "X",
                       values_to = varname) |>
  mutate(year = as.numeric(year)) |>
  arrange(Country.Name, year)
  return(data)
}

clean_under5 <- clean_data(clean_under5, 'Under 5 mortality rate')
clean_infant <- clean_data(clean_infant, 'Infant mortality rate')
clean_neonatal <- clean_data(clean_neonatal, 'Neonatal mortality rate')
clean_matmor <- clean_data(clean_matmor, 'Maternal Mortality rate')

#put all data frames into a list
list <- list(clean_infant, clean_neonatal, clean_under5, clean_matmor)

#merge four data sets to create one data set 
list |> reduce(full_join, by = c('Country.Name', 'year')) -> merged_data

#add ISO-3 to data
merged_data$ISO <- countrycode(merged_data$Country.Name, 
                               origin = "country.name", 
                               destination = "iso3c")
merged_data <- merged_data |>
  dplyr::select(-Country.Name)

#save the new dataset
write.csv(merged_data, file = "merged_mortality_data.csv")
