library(openxlsx)
library(dplyr)


#Reading file

missing_values_women_in_parliament <- read.xlsx("women_in_parliament_missing_values.xlsx")

#Changing name for merge (did not work)

missing_values_women_in_parliament <- missing_values_women_in_parliament %>% 
  rename(year = Year,
        Prop_Women = Percent) 

missing_values_women_in_parliament$Prop_Women <- as.double(missing_values_women_in_parliament$Prop_Women)

missing_values_women_in_parliament <- missing_values_women_in_parliament %>% 
  filter(Prop_Women != 0)




# Step 1: Perform a left join to match rows based on 'iso2c' and 'year' while bringing in 'Prop_Women' from missing_values_women_in_parliament
temp_df <- left_join(All_count_Res_1997, missing_values_women_in_parliament, by = c("iso2c", "year"), suffix = c("", ".from_missing"))

# Step 2: Update 'Prop_Women' in All_count_Res_1997 with non-NA 'Prop_Women.from_missing' values
temp_df <- temp_df %>%
  mutate(Prop_Women = if_else(!is.na(Prop_Women.from_missing), Prop_Women.from_missing, Prop_Women)) %>%
  select(-Prop_Women.from_missing) # Remove the temporary column


# Women in Parliament missing values
WP_NA_No_Alt_Data <- temp_df[is.na(temp_df$Prop_Women), ]







############## GDP PER CAPITA #################################################


Venezuela_GDP_per_Cap_2012_2022 <- read.xlsx("Venezuela real GDP.xlsx")

Venezuela_GDP_per_Cap_2012_2022$GDP_Growth_Cap <- as.numeric(Venezuela_GDP_per_Cap_2012_2022$GDP_Growth_Cap)

Venezuela_GDP_per_Cap_2012_2022 <- Venezuela_GDP_per_Cap_2012_2022 %>% 
  select(-Growth)

Venezuela_GDP_per_Cap_2012_2022$year <- as.numeric(Venezuela_GDP_per_Cap_2012_2022$year)


merge_VE <- left_join(Venezuela_GDP_per_Cap_2012_2022, VE_pop_res, by = "year")

merge_VE$Real_GDP_Growth_Cap <- merge_VE$GDP_Growth_Cap / merge_VE$SP.POP.TOTL


merge_VE <- merge_VE %>% 
  select(-GDP_Growth_Cap
         ,-iso3c
         ,-SP.POP.TOTL
         ,-country)




# Above is complete, just adding values below to temp_df


merge_VE <- merge_VE %>% 
  rename(GDP_Growth_Cap= Real_GDP_Growth_Cap)

# Assuming 'temp_df' and 'merge_VE' are already loaded into your R environment

# Perform a full join to ensure all data is available for comparison
full_df <- merge(temp_df, merge_VE, by = c("iso2c", "year"), all = TRUE, suffixes = c("", ".from_merge_VE"))

# For the specific case of filling in missing GDP_Growth_Cap values in temp_df from merge_VE
full_df$GDP_Growth_Cap <- ifelse(is.na(full_df$GDP_Growth_Cap), full_df$GDP_Growth_Cap.from_merge_VE, full_df$GDP_Growth_Cap)

# Now, remove the auxiliary columns brought in from merge_VE that you don't need
# This step depends on the names of the columns. Assuming 'GDP_Growth_Cap.from_merge_VE' is the only one you created
full_df$GDP_Growth_Cap.from_merge_VE <- NULL

# Optionally, if there were other variables from merge_VE that got merged and you don't need them, remove them as well
# full_df$OtherColumn.from_merge_VE <- NULL



temp_VE_df <- full_df

rm(full_df)
############Removing Cuba######################################


#Using filter to remove rows where iso2c equals "CU"
temp_VE_df <- temp_VE_df %>% filter(iso2c != "CU")




#################################################################


#Removing countries that have been justified to be remvoed



# Remove rows where iso2c is "AL", "BY", "HT", "ME", or "RS"
temp_VE_Avg_df <- temp_VE_df %>% 
  filter(!(iso2c %in% c("AL", "BY", "HT", "ME", "RS") & is.na(Prop_Women)))




#Filling in Bolivia

temp_VE_Avg_df[temp_VE_Avg_df$iso2c == "BO" & (temp_VE_Avg_df$year %in% c(1997, 1998)), "Prop_Women"] <- 11.53846

###############################################################

########## Filling in averages in NA's

fill_na_with_average <- function() {
  # Access the global dataframe
  for (i in 2:(nrow(temp_VE_Avg_df) - 1)) {
    # Check if the value in Prop_Women is NA
    if (is.na(temp_VE_Avg_df[i, "Prop_Women"])) {
      # Calculate the average of the previous and next non-NA values
      prev_value <- temp_VE_Avg_df[i - 1, "Prop_Women"]
      next_value <- temp_VE_Avg_df[i + 1, "Prop_Women"]
      if (!is.na(prev_value) & !is.na(next_value)) {
        temp_VE_Avg_df[i, "Prop_Women"] <- mean(c(prev_value, next_value))
      }
    }
  }
  # If the first or last row is NA, they remain as NA because there's no previous or next value to average
  return(temp_VE_Avg_df)
}

# After defining the function, you can call it without any arguments
temp_VE_Avg_df <- fill_na_with_average()




