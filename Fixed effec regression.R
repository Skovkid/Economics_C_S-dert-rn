#install.packages("broom")
#install.packages("stargazer")
#install.packages("plm")

library(stargazer)
library(plm)
library(broom)
library(dplyr)
library(ggplot2)


######################## ONLY GDP PER CAPITA (NO GROWTH)#########################



# Assuming Reg_w_NA_df_No_TM is your panel data and already prepared
fe_No_TM <- plm(GDP_Growth_Cap ~ Prop_Women + Trade + Capital_formation + WBL_ind,
                data = Reg_w_NA_df_No_TM,
                model = "within")



# Extract the fixed effects (country-specific intercepts)
fixed_effects_No_TM <- fixef(fe_No_TM)

# Convert to a data frame and view
fixed_effects_df_NT <- as.data.frame(fixed_effects_No_TM)
names(fixed_effects_df_NT) <- c("Fixed_Effect")
fixed_effects_df_NT$iso2c <- rownames(fixed_effects_df_NT)
rownames(fixed_effects_df_NT) <- NULL  # Clean up row names

# View the fixed effects for each country
fixed_effects_df_NT

#
summary(fe_No_TM)

# Extract model coefficients to a dataframe
model_coefficients_No_TM <- summary(fe_No_TM)$coef
model_coefficients_df_No_TM <- as.data.frame(model_coefficients_No_TM)
model_coefficients_df_No_TM$Variable <- rownames(model_coefficients_df_No_TM)
rownames(model_coefficients_df_No_TM) <- NULL  # Clean up row names

# View model coefficients for each variable
model_coefficients_df_No_TM



# Create the table with stargazer
stargazer(fe_No_TM, type = "text", 
          title = "Results of Fixed Effects Regression", 
          align = TRUE, 
          out = "table_FE_Regression.html")







####################################### GDP PER CAPITA GROWTH##################





# Assuming Reg_w_NA_df_No_TM is your panel data and already prepared
fe_No_TM_growth <- plm(GDP_Per_Capita_Growth ~ Prop_Women + Trade + Capital_formation + WBL_ind,
                data = Reg_w_NA_df_No_TM,
                model = "within")



# Extract the fixed effects (country-specific intercepts)
fixed_effects_No_TM_Growth <- fixef(fe_No_TM_growth)

# Convert to a data frame and view
fixed_effects_df_NT_Growth <- as.data.frame(fixed_effects_No_TM_Growth)
names(fixed_effects_df_NT_Growth) <- c("Fixed_Effect")
fixed_effects_df_NT_Growth$iso2c <- rownames(fixed_effects_df_NT_Growth)
rownames(fixed_effects_df_NT_Growth) <- NULL  # Clean up row names

# View the fixed effects for each country
fixed_effects_df_NT_Growth

#
summary(fe_No_TM_growth)

# Extract model coefficients to a dataframe
model_coefficients_No_TM <- summary(fe_No_TM_growth)$coef
model_coefficients_df_No_TM <- as.data.frame(model_coefficients_No_TM)
model_coefficients_df_No_TM$Variable <- rownames(model_coefficients_df_No_TM)
rownames(model_coefficients_df_No_TM) <- NULL  # Clean up row names

# View model coefficients for each variable
model_coefficients_df_No_TM



# Create the table with stargazer
stargazer(fe_No_TM_growth, type = "text", 
          title = "Results of Fixed Effects Regression", 
          align = TRUE, 
          out = "table_FE_Regression.html")



#Real GDP per Capita
stargazer(fe_No_TM_1, fe_No_TM_2, fe_No_TM_3, fe_No_TM,
          type = "text",  # Ensure type is set to "html" for HTML output
          title = "Results of Fixed Effects Regression",
          align = TRUE,
          out = "table_FE_Regression.txt")



#Real GDP per Capita Growth

stargazer(fe_No_TM_1_growth, fe_No_TM_2_growth, fe_No_TM_growth,
          type = "text",  # Ensure type is set to "html" for HTML output
          title = "Results of Fixed Effects Regression",
          align = TRUE,
          out = "table_FE_Regression.txt")

#################### One variable at a time #########################################

#Assuming Reg_w_NA_df_No_TM is your panel data and already prepared
fe_No_TM_1 <- plm(GDP_Growth_Cap ~ Prop_Women,
                data = Reg_w_NA_df_No_TM,
                model = "within")
summary(fe_No_TM_1)


###############################################################################

fe_No_TM_2 <- plm(GDP_Growth_Cap ~ Prop_Women + Trade,
                  data = Reg_w_NA_df_No_TM,
                  model = "within")
summary(fe_No_TM_2)



###############################################################################


fe_No_TM_3 <- plm(GDP_Growth_Cap ~ Prop_Women + Trade + Capital_formation,
                  data = Reg_w_NA_df_No_TM,
                  model = "within")
summary(fe_No_TM_3)


###############################################################################

# Assuming Reg_w_NA_df_No_TM is your panel data and already prepared
fe_No_TM_4 <- plm(GDP_Growth_Cap ~ Prop_Women + WBL_ind + Trade + Capital_formation,
                data = Reg_w_NA_df_No_TM,
                model = "within")

summary(fe_No_TM_4)

###############################################################################


main_df <- Reg_w_NA_df_No_TM


################## Dummy variables


main_df_with_dummies <- main_df %>%
  left_join(Reg_dum, by = "iso2c")

library(dplyr)
library(plm)

# Assuming pdata is your panel data frame already prepared for analysis

pdata <- main_df_with_dummies

# Create interaction terms
pdata <- pdata %>%
  mutate(
    GDP_dummy_latin_america = GDP_Growth_Cap * dummy_latin_america,
    GDP_dummy_us_canada = GDP_Growth_Cap * dummy_us_canada,
    GDP_dummy_europe = GDP_Growth_Cap * dummy_europe,
    GDP_dummy_russia_central_asia = GDP_Growth_Cap * dummy_russia_central_asia
  )

# Convert pdata to a pdata.frame for plm if not already done
pdata <- pdata.frame(pdata, index = c("iso2c", "year"))

# Now run the fixed effects regression with the interaction terms
fe_model_with_interaction <- plm(GDP_Growth_Cap ~ Prop_Women + Trade + Capital_formation + WBL_ind + 
                                   GDP_dummy_latin_america + GDP_dummy_us_canada + 
                                   GDP_dummy_europe + GDP_dummy_russia_central_asia,
                                 data = pdata, model = "within")

# Output the summary of the model
summary(fe_model_with_interaction)



# Create the table with stargazer
stargazer(fe_model_with_interaction, type = "text", 
          title = "Results of Fixed Effects Regression", 
          align = TRUE, 
          out = "table_FE_Regression.html")


############################ DO MORE WOMEN LEAD TO MORE WBL?#####################

Wom_n_WBL <- plm(WBL_ind ~ Prop_Women, data=pdata, model="within")
summary(Wom_n_WBL)


Wom_n_WBL_rev <- plm(Prop_Women ~ WBL_ind, data=pdata, model="within")
summary(Wom_n_WBL_rev)





#########Real GDP per Capita Growth


#################### One variable at a time #########################################

#Assuming Reg_w_NA_df_No_TM is your panel data and already prepared
fe_No_TM_1_growth <- plm(GDP_Per_Capita_Growth ~ Prop_Women,
                  data = Reg_w_NA_df_No_TM,
                  model = "within")

fe_No_TM_2_growth <- plm(GDP_Per_Capita_Growth ~ Prop_Women + Trade,
                  data = Reg_w_NA_df_No_TM,
                  model = "within")

fe_No_TM_3_growth <- plm(GDP_Per_Capita_Growth ~ Prop_Women + Trade + Capital_formation,
                  data = Reg_w_NA_df_No_TM,
                  model = "within")

