install.packages("broom")

library(stargazer)
library(plm)
library(broom)
library(dplyr)
library(ggplot2)



# Assuming Reg_w_NA_df_No_TM is your panel data and already prepared
fe_No_TM <- plm(GDP_Per_Capita_Growth ~ Prop_Women + Trade + Capital_formation + WBL_ind,
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





#################### One variable at a time #########################################

#Assuming Reg_w_NA_df_No_TM is your panel data and already prepared
fe_No_TM_1 <- plm(GDP_Per_Capita_Growth ~ Prop_Women,
                data = Reg_w_NA_df_No_TM,
                model = "within")
summary(fe_No_TM_1)


###############################################################################

fe_No_TM_2 <- plm(GDP_Per_Capita_Growth ~ Prop_Women + Trade,
                  data = Reg_w_NA_df_No_TM,
                  model = "within")
summary(fe_No_TM_2)



###############################################################################


fe_No_TM_3 <- plm(GDP_Per_Capita_Growth ~ Prop_Women + Trade + Capital_formation,
                  data = Reg_w_NA_df_No_TM,
                  model = "within")
summary(fe_No_TM_3)


###############################################################################

# Assuming Reg_w_NA_df_No_TM is your panel data and already prepared
fe_No_TM_4 <- plm(GDP_Per_Capita_Growth ~ Prop_Women + WBL_ind + Trade + Capital_formation,
                data = Reg_w_NA_df_No_TM,
                model = "within")
summary(fe_No_TM_4)

###############################################################################


