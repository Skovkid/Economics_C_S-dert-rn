library("tidyverse")

#Group by Isocode 
result_latin_america <- result_latin_america %>%
  arrange(iso2c, year)



#Filling in proportion women column with the data from the previous 
result_latin_america <- result_latin_america %>% 
  group_by(iso2c) %>%
  fill(PropWomen, .direction = "down") %>%
  ungroup()



###########################  Filling in Venezuela

# Example vector with hypothetical GDP values for Venezuela from 2016 to 2023
gdp_values_Venezuela <- c(4096.97,
                3676.38,
                3806.66,
                3529.72,
                2624.41,
                1566.60,
                2090.40,
                3421.75) # Replace with actual values

# Replace NA values for VE from 2016 to 2023
result_latin_america$GDPCap[result_latin_america$iso2c == "VE" & result_latin_america$year %in% 2015:2022] <- gdp_values_Venezuela

# You may need to ensure that only NAs are replaced and that the years align properly.
# If the years do not align, you will need to match them specifically.



##################### Handling Cubas missing GDPperCapita


# Assuming 'df' is your dataframe and it contains columns 'year' and 'GDPCap' for GDP per capita.

# Function to predict GDP per capita
predict_gdp_Cuba <- function(result_latin_america, new_years) {
  # Filter data for Cuba
  cuba_data <- result_latin_america[result_latin_america$iso2c == "CU", ]
  
  # Fit a linear model
  pred_CU <- lm(GDPCap ~ year, data = cuba_data)
  
  # Make predictions for new years
  predictions_cuba <- predict(pred_CU, newdata = data.frame(year = new_years))
  
  return(predictions_cuba)
}

# Predicting GDP for 2021 and 2022
CU_predicted_gdp_2021_2022 <- predict_gdp_Cuba(result_latin_america, c(2021, 2022))

