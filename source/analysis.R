# Load essentials for assignment
incarceration_trends <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")
library("dplyr")
library("ggplot2")

# Introduction + Summary Information: 5 relevant values of interest

# 1. What is the average population of females in 2018?
total_female_avg <- incarceration_trends %>%
  select(female_jail_pop, state, year) %>% 
  filter(year == 2018) %>% 
  group_by(state) %>%
  summarize(total_state = mean(female_jail_pop, na.rm = T)) %>%
  summarize(total = sum(total_state, na.rm = T)) %>%
  pull(total)

# 2. What is the average population of males in 2018
total_male_avg <- incarceration_trends %>%
  select(male_jail_pop, state, year) %>% 
  filter(year == 2018) %>% 
  group_by(state) %>%
  summarize(total_state = mean(male_jail_pop, na.rm = TRUE)) %>%
  summarize(total = sum(total_state, na.rm = TRUE)) %>%
  pull(total)

# 3. What is the difference between female average and male average in 2018?
difference_male_female <- incarceration_trends %>%
  filter(year == 2018) %>%
  group_by(state) %>%
  summarize(total_state = sum(total_jail_pop, na.rm = TRUE)) %>%
  summarize(difference = total_male_avg - total_female_avg) %>% 
  pull(difference)
  
# 4. What is the average population of black people in 2018?
total_black_avg <- incarceration_trends %>%
  select(black_jail_pop, state, year) %>% 
  filter(year == 2018) %>% 
  group_by(state) %>%
  summarize(total_state = mean(black_jail_pop, na.rm = T)) %>%
  summarize(total = sum(total_state, na.rm = T)) %>%
  pull(total)

# 5. What is the average population of white people in 2018?
total_white_avg <- incarceration_trends %>%
  select(white_jail_pop, state, year) %>% 
  filter(year == 2018) %>% 
  group_by(state) %>%
  summarize(total_state = mean(white_jail_pop, na.rm = T)) %>%
  summarize(total = sum(total_state, na.rm = T)) %>%
  pull(total)

# 6. What is the difference between black average and white average in 2018?
difference_black_white <- incarceration_trends %>%
  filter(year == 2018) %>%
  group_by(state) %>%
  summarize(total_state = sum(total_jail_pop, na.rm = TRUE)) %>%
  summarize(difference = total_white_avg - total_black_avg) %>% 
  pull(difference)

