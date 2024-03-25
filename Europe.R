library(dplyr)

# Define the ISO 2-letter country codes for the EU countries, the UK, and Iceland
include_countries_iso2c <- c("AT", "BE", "BG", "HR", "CY", "CZ", "DK", "EE", "FI", "FR", "DE", "GR", "HU", "IE", 
                             "IT", "LV", "LT", "LU", "MT", "NL", "PL", "PT", "RO", "SK", "SI", "ES", "SE", 
                             "GB", "IS")

# Filter to include only these countries in the 'result' dataframe
result_eu_uk_is <- result %>%
  filter(iso2c %in% include_countries_iso2c)

# 'result_eu_uk_is' now contains only data for the specified EU countries, the UK, and Iceland
