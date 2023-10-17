#write an R script that creates a figure that shows the trend in maternal mortality in countries that had an increase from 2000 to 2017

#load libraries
library(tidyr)
library(dplyr)

#load final data 
final_data <- read.csv(here("clean", "finaldata.csv"))

#make df for fig
fig_data <- final_data %>%
  dplyr::select(ISO, year, Maternal.Mortality.rate) %>%
  filter(year < 2018) %>%
  group_by(ISO) %>%
  #create new variable that gives the difference between each year and baseline year
  mutate(diffmatmor = Maternal.Mortality.rate - Maternal.Mortality.rate[1L]) %>%
  #resort the years into descending order
  arrange(ISO, desc(year)) %>%
  #if first diffmatmort is greater than 1, then, then incmatmor=1
  mutate(incmatmor = ifelse(diffmatmor[1L] > 0, 1, 0)) %>%
  #resort to ascending year
  arrange(ISO, year) %>%
  ungroup() %>%
  #now only include countries that had an increase in matmor since 2000
  dplyr::filter(incmatmor == 1)

#number of unique countries
length(unique(fig_data$ISO)) #output says 13 countries
  
#make fig
fig1 <- fig_data |>
  ggplot(aes(x = year, y = Maternal.Mortality.rate, group = ISO)) +
  geom_line(aes(color = ISO), alpha = 1, linewidth = 1) +
  xlim(c(2000,2017)) +
  # use log 10 sclae for y axis
  scale_y_continuous(trans='log10') + 
  labs(y = "Maternal mortality (log 10 scale)", x = "Year", color = "Country", title = "Trend in maternal mortality for countries that had an increase from 2000 to 2017") + 
  # use black and white theme and increase the size of labels
  theme_bw(base_size = 12)


  #include only the countries whose matmor increased from 2000 to 2017
  dplyr::filter(incmatmor == 1)
  #resort data by country and decreasing year
  arrange(ISO, year) %>%
  ungroup() %>%
  
  # only select variables needed to create the figure
  :select(country_name, ISO, year, matmor) |>
  # select years 2000 to 2017
  dplyr::filter(year < 2018) |>
  # gropu by country
  group_by(ISO) |>
  # create a new variable diffmatmor that takes the difference between each year and baseline year 
  mutate(diffmatmor = matmor - matmor[1L]) |>
  # sort the data by country and decreasing year
  arrange(ISO, desc(year)) |>
  # if the first difference is greater than 0 (if year 2017 is greater than baseline, then incmatmor = 1)
  mutate(incmatmor = ifelse(diffmatmor[1L] > 0 , 1, 0)) |>
  # re-sort data by country and increasing year
  arrange(ISO, year) |>
  ungroup() |>
  # only include countries that had an increase in maternal mortality from 2000 to 2017
  dplyr::filter(incmatmor == 1)
length(unique(forfigure$ISO))