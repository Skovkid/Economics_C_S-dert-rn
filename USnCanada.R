library(dplyr)

# Filter to include only the United States and Canada
result_us_canada <- result %>%
  filter(iso2c %in% c("US", "CA"))

# 'result_us_canada' now contains only data for the United States and Canada
