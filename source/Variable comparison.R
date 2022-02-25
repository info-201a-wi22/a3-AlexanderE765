# Load essentials for assignment
incarceration_trends <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")
library("dplyr")
library("ggplot2")

# Compare men in jail to women in jail with a scatter plot.
men_women_jail <- incarceration_trends %>%
  select(male_jail_pop, female_jail_pop)

# Create scatter plot
var_comp_chart <- ggplot(men_women_jail) +
  geom_point(
    mapping = aes(y = female_jail_pop, x = male_jail_pop),
    color = "red"
  ) +
  labs(
    title = "NUMBER OF WOMEN IN JAIL FOR EVERY MAN",
    x = "Men jail population",
    y = "Women jail population"
  )
plot(var_comp_chart)
