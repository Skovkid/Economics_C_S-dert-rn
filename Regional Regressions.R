library(plm)
library(dplyr)
library(stargazer)

# Example subsets and regressions for each region
# Ensure that 'region' or the appropriate dummy variables are correctly defined in your dataset


###################################### JUST REAL GDP######################
# For Europe
europe_data <- filter(main_df_with_dummies, region == "Europe")
model_europe <- plm(GDP_Growth_Cap ~ Prop_Women + Trade + Capital_formation + WBL_ind,
                    data = europe_data, model = "within")
summary(model_europe)


# For Latin America
latin_america_data <- filter(main_df_with_dummies, region == "Latin America")
model_latin_america <- plm(GDP_Growth_Cap ~ Prop_Women + Trade + Capital_formation + WBL_ind,
                           data = latin_america_data, model = "within")
summary(model_latin_america)

# For US and Canada
us_canada_data <- filter(main_df_with_dummies, region == "US and Canada")
model_us_canada <- plm(GDP_Growth_Cap ~ Prop_Women + Trade + Capital_formation + WBL_ind,
                       data = us_canada_data, model = "within")
summary(model_us_canada)

# For Russia and Central Asia
russia_central_asia_data <- filter(main_df_with_dummies, region == "Russia and Central Asia")
model_russia_central_asia <- plm(GDP_Growth_Cap ~ Prop_Women + Trade + Capital_formation + WBL_ind,
                                 data = russia_central_asia_data, model = "within")
summary(model_russia_central_asia)



################################## JUST GDP GROWHT ############################

# For Europe
model_europe_growth <- plm(GDP_Per_Capita_Growth ~ Prop_Women + Trade + Capital_formation + WBL_ind,
                    data = europe_data, model = "within")
summary(model_europe_growth)

# For Latin America

model_latin_america_growth <- plm(GDP_Per_Capita_Growth ~ Prop_Women + Trade + Capital_formation + WBL_ind,
                           data = latin_america_data, model = "within")
summary(model_latin_america_growth)

# For US and Canada

model_us_canada_growth <- plm(GDP_Per_Capita_Growth ~ Prop_Women + Trade + Capital_formation + WBL_ind,
                       data = us_canada_data, model = "within")
summary(model_us_canada_growth)

# For Russia and Central Asia

model_russia_central_asia_growth <- plm(GDP_Per_Capita_Growth ~ Prop_Women + Trade + Capital_formation + WBL_ind,
                                 data = russia_central_asia_data, model = "within")
summary(model_russia_central_asia_growth)




########################################## Seeing WBL in different regions

#Var name Feml_lawmke_reg


#For Europe
Feml_lawmke_europe <- plm(Prop_Women ~ WBL_ind, data=europe_data, model="within")
summary(Feml_lawmke_europe)

#For Latin America
Feml_lawmke_reg_latam <- plm(Prop_Women ~ WBL_ind, data=latin_america_data, model="within")
summary(Feml_lawmke_reg_latam)

#For US and Canada
Feml_lawmke_reg_US_C <- plm(Prop_Women ~ WBL_ind, data=us_canada_data, model="within")
summary(Feml_lawmke_reg_US_C)

#For Russia and central asia

Feml_lawmke_reg_Ru <-plm(Prop_Women ~ WBL_ind, data=russia_central_asia_data, model="within")
summary(Feml_lawmke_reg_Ru)


##############################



stargazer(model_europe, model_latin_america,model_us_canada,model_russia_central_asia,
          type = "text",  # Ensure type is set to "html" for HTML output
          title = "Results of Fixed Effects Regression",
          align = TRUE,
          out = "table_FE_Regression.txt",
          column.labels = c("Europe", "Latina America", "US Canada", "Russia  CA"))

# Use stargazer to create a summary table for the models
stargazer(model_europe_growth, model_latin_america_growth, model_us_canada_growth, model_russia_central_asia_growth,
          type = "text",
          title = "Results of Fixed Effects Regression",
          align = TRUE,
          column.labels = c("Europe", "Latin America", "US & Canada", "Russia & Central Asia"),
          out = NULL)  # Display in R console first to ensure no errors

# Capturing the output
output_text_growth <- stargazer(model_europe_growth, model_latin_america_growth, model_us_canada_growth, model_russia_central_asia_growth,
                         type = "text",
                         title = "Results of Fixed Effects Regression",
                         align = TRUE,
                         column.labels = c("Europe", "Latin America", "US & Canada", "Russia & Central Asia"))

# Writing the output to a text file
writeLines(output_text_growth, "table_FE_Regression.txt")
