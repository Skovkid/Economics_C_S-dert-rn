library(dplyr)
library(openxlsx)



# Sovereign states in Latin America in ISO 3166-1 alpha-2
sovereign_latin_america_iso2c <- c(
  "AR", # Argentina
  "BO", # Bolivia
  "BR", # Brazil
  "CL", # Chile
  "CO", # Colombia
  "CR", # Costa Rica
  "CU", # Cuba
  "DO", # Dominican Republic
  "EC", # Ecuador
  "SV", # El Salvador
  "GT", # Guatemala
  "HT", # Haiti
  "HN", # Honduras
  "MX", # Mexico
  "NI", # Nicaragua
  "PA", # Panama
  "PY", # Paraguay
  "PE", # Peru
  "UY", # Uruguay
  "VE"  # Venezuela
)

# Sovereign states in the Caribbean in ISO 3166-1 alpha-2
sovereign_caribbean_iso2c <- c(
  "AG", # Antigua and Barbuda
  "BS", # Bahamas
  "BB", # Barbados
  "BZ", # Belize
  "DM", # Dominica
  "GD", # Grenada
  "GY", # Guyana
  "JM", # Jamaica
  "KN", # Saint Kitts and Nevis
  "LC", # Saint Lucia
  "VC", # Saint Vincent and the Grenadines
  "SR", # Suriname
  "TT"  # Trinidad and Tobago
)

# Combine both lists of ISO2C codes
sovereign_states_iso2c <- c(sovereign_latin_america_iso2c, sovereign_caribbean_iso2c)

# Create a dataframe from the combined list
sovereign_states_iso2c_df <- data.frame(iso2c = sovereign_states_iso2c)

# Now, you can use this dataframe to filter your 'result' dataframe:
# Assuming 'result' has a column named 'iso2c' and a column named 'year'

result_filtered <- result %>%
  filter(iso2c %in% sovereign_states_iso2c) %>%
  filter(year >= 1997)


# Assuming result_filtered is already filtered by year and contains a column named 'iso2c'

# Filter for Latin America
result_latin_america <- result_filtered %>%
  filter(iso2c %in% sovereign_latin_america_iso2c)

# Filter for the Caribbean
result_caribbean <- result_filtered %>%
  filter(iso2c %in% sovereign_caribbean_iso2c)




write.xlsx(result_filtered, file = "latam_car.xlsx")
