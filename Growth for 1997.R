library(dplyr)


Growth1997 <- GDP_Growth


Growth1997 <- Growth1997 %>% 
  filter(iso2c %in% All_countries_list)

Growth1997 <- Growth1997 %>% 
  filter(year >= 1996,
         year <= 1997)

Growth1997 <- Growth1997 %>%
  filter(!(iso2c == "CU" & year %in% c(1996, 1997)))  

Growth1997 <- Growth1997 %>%
  filter(!(iso2c == "ME" & year %in% c(1996, 1997)))  




#Create a lagged column manually
Growth1997 <- as.data.frame(Growth1997) %>%
  group_by(iso2c) %>%
  arrange(iso2c, year) %>%
  mutate(lagged_GDP = dplyr::lag(NY.GDP.PCAP.PP.CD)) %>%
  ungroup()

# Calculate the growth rate in a new step
Growth1997 <- Growth1997 %>%
  mutate(GDP_Per_Capita_Growth = ifelse(is.na(lagged_GDP), NA,
                                        (NY.GDP.PCAP.PP.CD - lagged_GDP) / lagged_GDP * 100))


Growth1997 <- Growth1997 %>% 
  filter(!(year == 1996))




