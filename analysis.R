# Loading packages
library("dplyr")
library("tidyr")
library("ggplot2")

# Reading dataset
incarceration_trends <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")
View(incarceration_trends)

# Summary Information

# 1) What is the highest amount of jailed Black people when looking at all counties in 2018
highest_blacks_jailed <- incarceration_trends %>%
  filter(year == 2018) %>%
  summarize(max_black_jail_pop = max(black_jail_pop, na.rm = TRUE))

# 2) What is the lowest amount of jailed Black people when looking at all counties in 2018
lowest_blacks_jailed <- incarceration_trends %>%
  filter(year == 2018) %>%
  summarize(min_black_jail_pop = min(black_jail_pop, na.rm = TRUE))

# 3) What is the average amount of jailed Black people when looking at all counties in 2018
avg_blacks_jailed <- incarceration_trends %>%
  filte(year == 2018) %>%
  summarize(avg_blacks_jailed = mean(black_jail_pop, na.rm = TRUE))

# 4) Which county had the highest amount of Black people in jail in 2018
highest_count_blacks_jailed <- incarceration_trends %>%
  filter(year == 2018) %>%
  group_by(county_name) %>%
  summarize(total_state_pop = sum(black_jail_pop, na.rm = TRUE)) %>%
  filter(total_state_pop == max(total_state_pop, na.rm = TRUE)) %>%
  pull(county_name)

