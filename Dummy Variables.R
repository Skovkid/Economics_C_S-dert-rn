library(dplyr)
library(tidyverse)

# Create an example data frame
Reg_dum <- data.frame(
  iso2c = c("AR", "BO", "BR", "CL", "CO", "CR", "CU", "DO", "EC", "SV",
            "GT", "HT", "HN", "MX", "NI", "PA", "PY", "PE", "UY", "VE",
            "US", "CA", "AT", "BE", "BG", "CH", "CY", "CZ", "DE",
            "DK", "EE", "ES", "FI", "FR", "GB", "GR",
            "HR", "HU", "IE", "IS", "IT", "LT", "LU",
            "LV", "MD", "ME", "MK", "MT", "NL", "NO",
            "PL", "PT", "RO", "RS", "SE", "SI", "SK", "UA",
            "RU", "KZ", "UZ", "KG", "TJ", "BY",
            "AL", "AM", "AZ", "GE", "TR", "TM")  # Ensure no trailing comma without following values
)

# Now perform the mutation to add region categories
Reg_dum <- Reg_dum %>%
  mutate(
    region = case_when(
      iso2c %in% c("AR", "BO", "BR", "CL", "CO", "CR", "CU", "DO", "EC", "SV",
                   "GT", "HT", "HN", "MX", "NI", "PA", "PY", "PE", "UY", "VE") ~ "Latin America",
      iso2c %in% c("US", "CA") ~ "US and Canada",
      iso2c %in% c("AT", "BE", "BG", "CH", "CY", "CZ", "DE",
                   "DK", "EE", "ES", "FI", "FR", "GB", "GR",
                   "HR", "HU", "IE", "IS", "IT", "LT", "LU",
                   "LV", "MD", "ME", "MK", "MT", "NL", "NO",
                   "PL", "PT", "RO", "RS", "SE", "SI", "SK", "UA", "AL","TR") ~ "Europe",
      iso2c %in% c("RU", "KZ", "UZ", "KG", "TJ", "BY","TM","AZ", "GE","AM") ~ "Russia and Central Asia",
      TRUE ~ "Other"
    )
  )


# Create dummy variables for each region
Reg_dum <- Reg_dum %>%
  mutate(
    dummy_latin_america = as.integer(region == "Latin America"),
    dummy_us_canada = as.integer(region == "US and Canada"),
    dummy_europe = as.integer(region == "Europe"),
    dummy_russia_central_asia = as.integer(region == "Russia and Central Asia")
  )


