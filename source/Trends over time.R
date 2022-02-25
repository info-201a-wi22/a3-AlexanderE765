# Load necessities
library("dplyr")
library("ggplot2")
library("tidyverse")
incarceration_trends <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

# Create a chart showing each race group in jail in King County from 2010 to 2018

# Wrangling Data 
incarceration_trends_over_time <- incarceration_trends %>%
  select(year, county_name, total_jail_pop, aapi_jail_pop, native_jail_pop,
         black_jail_pop, white_jail_pop, other_race_jail_pop,
         latinx_jail_pop) %>%
  filter(county_name == "King County") %>%
  group_by(year) %>%
  summarize(
    total_black = sum(native_jail_pop, na.rm = TRUE),
    total_white = sum(aapi_jail_pop, na.rm = TRUE),
    total_native = sum(black_jail_pop, na.rm = TRUE),
    total_aapi = sum(white_jail_pop, na.rm = TRUE),
    total_latinx = sum(latinx_jail_pop, na.rm = TRUE),
    total_other = sum(other_race_jail_pop, na.rm = TRUE)
  ) %>%
  gather(key = race, value = population, -year) %>%
  filter(year >= 2010)
View(incarceration_trends_over_time)

# Create Chart 
trend_chart <- ggplot(data = incarceration_trends_over_time) +
  geom_area(
    mapping = aes(x = year, y = population, fill = race)
  ) +
  (scale_fill_brewer(palette = "Dark2")
  ) +
  labs (
    title = "NUMBER OF PEOPLE IN JAIL BY RACE IN KING COUNTY",
    subtitle = "2008 to 2018",
    fill = "Race",
    x = "Year",
    y = "Population"
  )
plot(trend_chart)
