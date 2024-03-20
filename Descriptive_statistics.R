# Visualize missing data over time
library(ggplot2)
library(tidyr)

result_latin_america %>%
  gather(key = "variable", value = "value", -iso2c, -year) %>%
  mutate(missing = is.na(value)) %>%
  ggplot(aes(x = year, y = iso2c, fill = missing)) +
  geom_tile() +
  scale_fill_manual(values = c("reported" = "blue", "missing" = "grey")) +
  labs(fill = "Data Status",
       x = "Year",
       y = "Country ISO2 Code") +
  theme_minimal()

# Analyze patterns of missing data
library(naniar)
result_latin_america %>%
  is.na() %>%
  colSums()
