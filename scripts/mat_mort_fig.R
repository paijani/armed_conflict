#write an R script that creates a figure that shows the trend in maternal mortality in countries that had an increase from 2000 to 2017

#load libraries
library(tidyr)
library(dplyr)

#load final data 
final_data <- read.csv(here("clean", "final_data.csv"))

#make df for fig
fig_data <- final_data %>%
  dplyr::select(country_name, ISO, year, Maternal.Mortality.rate) %>%
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
fig1 <- fig_data %>%
  ggplot(aes(x = year, y = Maternal.Mortality.rate, group = ISO)) +
  geom_line(aes(color = country_name), alpha = 1, linewidth = 1) +
  xlim(c(2000,2017)) +
  # use log 10 scale for y axis
  scale_y_continuous(trans='log10') + 
  labs(y = "Maternal Mortality (log 10 scale)", x = "Year", color = "Country", title = "Trends in maternal mortality for countries that had an increase from 2000 to 2017") + 
  # use black and white theme and increase the size of labels
  theme_bw(base_size = 12)


fig1

#save plot as png
ggsave(fig1, file = here("figures", "fig1_matmor.png"), width = 8, height = 5)
  