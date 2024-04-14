library(ggplot2)
library(maps)
library(dplyr)

########################MAKE WORLD MAP######################################

install.packages("countrycode")
library(countrycode)


# Load world map data

world_map <- map_data("world")


# Convert the 'region' column to ISO 2-letter country codes

world_map$iso2c <- countrycode(world_map$region, "country.name", "iso2c")

# Note: 'country.name' is the origin code format in the world_map, and 'iso2c' is the destination code format.


world_map <- world_map %>% 
  select(-subregion,-region)






########################## Presentation ########################################



# Merge model results with the world map data
world_map_with_results <- world_map %>%
  full_join(world_map, model_results, by = "iso2c")


# Plot the map with regression estimates
ggplot() +
  geom_polygon(data = world_map_with_results, aes(x = long, y = lat, group = group, fill = estimate), color = "white") +
  scale_fill_continuous(low = "blue", high = "red", na.value = "grey50", name = "Coefficient") +
  labs(title = "Regression Coefficients by Country") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  coord_fixed(1.3)  # To preserve aspect ratio
