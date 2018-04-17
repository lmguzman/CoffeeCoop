library(tidyverse)

hist.data <- read.csv(file = 'coffee_database/consumption.csv')

hist.data.subset <- hist.data %>% 
  filter(Date == "2018-04-16")

hist(hist.data.subset$Coffee)

ggplot(data=hist.data.subset, aes(hist.data.subset$Coffee)) + 
  geom_histogram() + 
  theme_bw()
