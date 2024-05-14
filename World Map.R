install.packages("maps", dependencies = TRUE, INSTALL_opts = '--no-lock')
install.packages("sf", dependencies = TRUE, INSTALL_opts = '--no-lock')
install.packages("worldmap")

library(ggplot2)
library(worldmap)
library(dplyr)
library(sf)
library(rnaturalearth)
install.packages("countrycode")
library(countrycode)
########################MAKE WORLD MAP######################################

# Load world map data

world_map <- map_data("world")


# Convert the 'region' column to ISO 2-letter country codes

world_map$iso2c <- countrycode(world_map$region, "country.name", "iso2c")

# Note: 'country.name' is the origin code format in the world_map, and 'iso2c' is the destination code format.


world_map <- world_map %>% 
  select(-subregion,-region)


world <- ne_countries(scale = "small", returnclass = "sf")

######################################

world_map <- map_data("world")

world_map <- world_map %>% 
  rename(country = region)

 
# May need to adjust country names to match map data, here's a basic example
country_names <- countrycode::countrycode(fixed_effects_df_NT_Growth$iso2c, "iso2c", "country.name")
fixed_effects_df_NT_Growth$country <- country_names




#Renaming for perfect match
fixed_effects_df_NT_Growth <- fixed_effects_df_NT_Growth %>%
  mutate(country = case_when(
    country == "United States" ~ "USA",
    country == "United Kingdom" ~ "UK",
    country == "Czechia" ~ "Czech Republic", 
    TRUE ~ country  # Default to leave the country name as is
  ))




# Merge the data
map_data_merged <- left_join(world_map, fixed_effects_df_NT_Growth, by = "country")


ggplot(data = map_data_merged, aes(x = long, y = lat, group = group, fill = Fixed_Effect)) +
  geom_polygon(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, 
                       limit = c(min(map_data_merged$Fixed_Effect, na.rm = TRUE), 
                                 max(map_data_merged$Fixed_Effect, na.rm = TRUE)), 
                       name = "Fixed Effect") +
  labs(title = "Country-specific Fixed Effects on Growth") +
  theme_minimal()



#Better map?

# Plotting the data with NA values set to transparent
ggplot(data = map_data_merged, aes(x = long, y = lat, group = group, fill = Fixed_Effect)) +
  geom_polygon(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0,
                       na.value = NA,  # Set NA fill color to transparent
                       limit = c(min(map_data_merged$Fixed_Effect, na.rm = TRUE),
                                 max(map_data_merged$Fixed_Effect, na.rm = TRUE)),
                       name = "Fixed Effect") +
  labs(title = "Country-specific Fixed Effects on Growth") +
  theme_minimal()


########################## Presentation ########################################

# Define breaks and colors
breaks <- c(-Inf, -5, 0, 5, Inf)  # Adjust these thresholds as needed
colors <- c("red", "white", "blue")

# Plotting the data with the new color scale
ggplot(data = map_data_merged, aes(x = long, y = lat, group = group, fill = Fixed_Effect)) +
  geom_polygon(color = "white") +
  scale_fill_gradientn(colours = colors, 
                       values = scales::rescale(c(-10, -0.01, 0, 0.01, 10)),  # Example values, adjust as necessary
                       limits = c(min(map_data_merged$Fixed_Effect, na.rm = TRUE), 
                                  max(map_data_merged$Fixed_Effect, na.rm = TRUE)),
                       na.value = NA,  # Set NA fill color to transparent
                       name = "Fixed Effect") +
  labs(title = "Country-specific Fixed Effects on Growth") +
  theme_minimal()
