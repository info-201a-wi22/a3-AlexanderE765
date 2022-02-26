# Load essentials for assignment
incarceration_trends <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")
library("dplyr")
library("stringr")
library("tidyverse")
library("ggplot2")
library("maps")

# Ratio of black population in prison compared to total population in prison in the U.S. in 2018
black_total_jail_ratio <- incarceration_trends %>%
  select(total_jail_pop, black_jail_pop, year, fips) %>%
  filter(year == 2018) %>%
  group_by(fips) %>%
  summarize(ratio_black_total_in_jail = black_jail_pop / total_jail_pop) %>%
  filter(ratio_black_total_in_jail <= 1)
View(black_total_jail_ratio)

# Load a shapefile of U.S. counties using ggplot's `map_data()` function
# Add a column named `polyname` so that it can be joined with county.fips
county <- map_data("county") %>%
  unite(polyname, region, subregion, sep = ",") %>%
  left_join(county.fips, by = "polyname")
View(county)

# Join `black_jail_percent` data to the U.S. shapefile. `joined_df`
dataframe_joined <- county %>%
  left_join(black_total_jail_ratio, by = "fips")

# Define a blank theme for map
blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),        
    axis.text = element_blank(),        
    axis.ticks = element_blank(),       
    axis.title = element_blank(),       
    plot.background = element_blank(),  
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.border = element_blank()      
  )

# Draw the map setting the `fill` of each county using their Black percent
map <- ggplot(dataframe_joined) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group,
                  fill = ratio_black_total_in_jail),
    size = .3
  ) +
  labs(
    fill = "Ratio",
    title = "Ratio of Black People in Jail in 2018",
    subtitle = "2018"
  ) +
  scale_fill_continuous(low = "#132B43", high = "#56B1F7") +
  coord_map(
  ) + 
  blank_theme
plot(map)
