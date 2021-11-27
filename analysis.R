# Loading packages
library("dplyr")
library("tidyr")
library("ggplot2")
library("lintr")
library("styler")
library("mapproj")

# Reading data set
incarceration_trends <- read.csv("https://raw.githubusercontent.com/
vera-institute/incarceration-trends/master/incarceration_trends.csv")

# Summary Information
# Here, I want to analyze the proportions for jailed White and Black people in
# regards to the population of each respective race in the most recent year
# in this data set

# 1) What is the most recent year in the data set
recent_year <- incarceration_trends %>%
  summarize(year = max(year, na.rm = TRUE)) %>%
  pull(year)

# 2) What county has the highest amount of jailed Black people
county_highest_pop <- incarceration_trends %>%
  filter(total_jail_pop == max(total_jail_pop, na.rm = TRUE)) %>%
  pull(county_name)

# 3) What is the Black population in county_highest_pop in 2018
black_population <- incarceration_trends %>%
  filter(year == recent_year, county_name == county_highest_pop) %>%
  pull(black_pop_15to64)

# 4) What is the White population in county_highest_pop in 2018
white_population <- incarceration_trends %>%
  filter(year == recent_year, county_name == county_highest_pop) %>%
  pull(white_pop_15to64)

# 5) How many Black people are jailed in county_highest_pop in recent_year
blacks_jailed <- incarceration_trends %>%
  filter(year == recent_year, county_name == county_highest_pop) %>%
  pull(black_jail_pop)

# 6) How many White people are jailed in county_highest_pop in recent_year
whites_jailed <- incarceration_trends %>%
  filter(year == recent_year, county_name == county_highest_pop) %>%
  pull(white_jail_pop)

# 7) What is the proportion of jailed Black people to the population in
# county_highest_pop in recent_year
blacks_jailed_proportion <- blacks_jailed / black_population

# 8) What is the proportion of jailed White people to the population in
# county_highest_pop in recent_year
whites_jailed_proportion <- whites_jailed / white_population

# Trends Over Time Chart
# Compare the proportion of jailed people of a certain race to the respective
# population between ages 15 and 64.
trend_over_time <- incarceration_trends %>%
  # Group by years
  group_by(year) %>%
  # Get population for:
  # 1) Black people between 15-64 yrs old
  # 2) White people between 15-64 yrs old
  # 3) Latinx people between 15-64 yrs old
  # 4) AAPI people between 15-64 yrs old
  # 5) Black people in jail
  # 6) White people in jail
  # 7) Latinx people in jail
  # 8) Latinx people in jail
  summarize(
    total_black_population = sum(black_pop_15to64, na.rm = TRUE),
    total_white_population = sum(white_pop_15to64, na.rm = TRUE),
    total_latinx_population = sum(latinx_pop_15to64, na.rm = TRUE),
    total_aapi_population = sum(aapi_pop_15to64, na.rm = TRUE),
    jailed_black_population = sum(black_jail_pop, na.rm = TRUE),
    jailed_white_population = sum(white_jail_pop, na.rm = TRUE),
    jailed_latinx_population = sum(latinx_jail_pop, na.rm = TRUE),
    jailed_aapi_population = sum(aapi_jail_pop, na.rm = TRUE),
  ) %>%
  # Get proportions for:
  # 1) Black people in jail with respect to race population 15-64 yrs old
  # 2) White people in jail with respect to race population 15-64 yrs old
  # 3) Latinx people in jail with respect to race population 15-64 yrs old
  mutate(
    black_proportion = jailed_black_population / total_black_population,
    white_proportion = jailed_white_population / total_white_population,
    latinx_proportion = jailed_latinx_population / total_latinx_population,
    aapi_proportion = jailed_aapi_population / total_aapi_population
  )

trend_chart <- ggplot(data = trend_over_time) +
  geom_line(mapping = aes(
    x = year, y = black_proportion,
    color = "Black"
  ), size = 2) +
  geom_line(mapping = aes(
    x = year, y = white_proportion,
    color = "White"
  ), size = 2) +
  geom_line(mapping = aes(
    x = year, y = latinx_proportion,
    color = "Latinx"
  ), size = 2) +
  geom_line(mapping = aes(
    x = year, y = aapi_proportion,
    color = "AAPI"
  ), size = 2) +
  ggtitle("Proportions Of Jailed People With Respect to Race and Population
Over Years") +
  labs(x = "Years", y = "Proportions Jailed")

trend_chart$labels$color <- "Race"

trend_chart

# Variable Comparison Chart
variable_compare_data <- incarceration_trends %>%
  group_by(year) %>%
  summarize(
    total_white_prison_rate = sum(white_prison_pop_rate, na.rm = TRUE),
    total_black_prison_rate = sum(black_prison_pop_rate, na.rm = TRUE)
  )

scatter_variable_data <- ggplot(variable_compare_data) +
  geom_point(aes(
    x = total_white_prison_rate,
    y = total_black_prison_rate
  )) +
  labs(
    title = "White Prision Population Rate vs Black Prision Population Rate",
    x = "White Prison Population Rate",
    y = "Black Prison Population Rate"
  )

scatter_variable_data

# Map
jail_rates <- incarceration_trends %>%
  select(
    county_name, year, state, fips, black_pop_15to64, white_pop_15to64,
    black_jail_pop, white_jail_pop
  ) %>%
  # Ratio of Black jailed people to White jailed people with respect to their
  # race population
  mutate(
    black_white_jail_ratio = (black_jail_pop / black_pop_15to64) /
      (white_jail_pop / white_pop_15to64)
  ) %>%
  filter(year == 2018)

counties <- map_data("county") %>%
  unite(polyname,
    region,
    subregion,
    sep = ","
  ) %>%
  left_join(county.fips, by = "polyname")

map_data <- counties %>%
  left_join(jail_rates, by = "fips") %>%
  filter(state == "CA" | state == "WA" | state == "OR")

theme <- theme_bw() +
  theme(
    plot.background = element_blank(),
    panel.border = element_blank(),
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid.major = element_blank(),
  )

map <- ggplot(map_data) +
  geom_polygon(
    mapping = aes(
      x = long,
      y = lat,
      group = group,
      fill = black_white_jail_ratio
    ),
    color = "White"
  ) +
  coord_map() +
  labs(fill = "Black-White Ratio") +
  ggtitle("Black to White Jailed Ratio") +
  theme

map

lint("analysis.R")
style_file("analysis.R")
