install.packages("stargazer")
install.packages("plm")
library(stargazer)
library(plm)
library(dplyr)

#Declaring the NA dataframe
Reg_w_NA_df<- temp_VE_Avg_df 


Reg_w_NA_df <- Reg_w_NA_df %>% 
  select(-country)


Reg_w_NA_df <- Reg_w_NA_df %>% 
  select(-GDP_Cap)



Reg_w_NA_df <- pdata.frame(Reg_w_NA_df, index = c("iso2c", "year"))


#################### ISSUE! #################


Reg_w_NA_df_No_TM <- Reg_w_NA_df



#Remove the Turkmenistan values

# Assuming Reg_w_NA_df_No_TM is your data frame 
Reg_w_NA_df_No_TM <- Reg_w_NA_df_No_TM %>%
  filter(!(iso2c == "TM" & year %in% c(2020, 2021, 2022)))


# Calculate GDP per capita growth rate


#Create a lagged column manually
Reg_w_NA_df_No_TM <- as.data.frame(Reg_w_NA_df_No_TM) %>%
  group_by(iso2c) %>%
  arrange(iso2c, year) %>%
  mutate(lagged_GDP = dplyr::lag(GDP_Growth_Cap)) %>%
  ungroup()

# Calculate the growth rate in a new step
Reg_w_NA_df_No_TM <- Reg_w_NA_df_No_TM %>%
  mutate(GDP_Per_Capita_Growth = ifelse(is.na(lagged_GDP), NA,
                                        (GDP_Growth_Cap - lagged_GDP) / lagged_GDP * 100))



#
#########  MERGE##############################################################


Growth1997_p_df <- pdata.frame(Growth1997, index = c("iso2c", "year"))

Growth1997_p_df <- Growth1997_p_df %>% 
  select(-iso3c,-NY.GDP.PCAP.PP.CD,-lagged_GDP)




#Matching data type
Reg_w_NA_df_No_TM$year <- as.integer(as.character(Reg_w_NA_df_No_TM$year))
Growth1997_p_df$year <- as.integer(as.character(Growth1997_p_df$year))
Reg_w_NA_df_No_TM$iso2c <- as.character(Reg_w_NA_df_No_TM$iso2c)
Growth1997_p_df$iso2c <- as.character(Growth1997_p_df$iso2c)


# Now perform the join
Reg_w_NA_df_No_TM <- Reg_w_NA_df_No_TM %>%
  left_join(Growth1997_p_df, by = c("iso2c", "year"), suffix = c("", ".1997"))



# Mutate into other column

# Assuming Reg_w_NA_df_No_TM is your main dataframe and already loaded into your R session
Reg_w_NA_df_No_TM <- Reg_w_NA_df_No_TM %>%
  mutate(GDP_Per_Capita_Growth = ifelse(is.na(GDP_Per_Capita_Growth) & year == 1997, 
                                        GDP_Per_Capita_Growth.1997, 
                                        GDP_Per_Capita_Growth))



Reg_w_NA_df_No_TM <- Reg_w_NA_df_No_TM %>% 
  select(-GDP_Per_Capita_Growth.1997)





##########################################################################








# Fit fixed effects model
fe_model <- plm( ~ , data = Reg_w_NA_df_No_TM)









summary(fe_model)



















# Regression 

model1 <- lm(GDPCap ~ independent_variable1, data = result_latin_america)

 
model2 <- lm(dependent_variable ~ independent_variable1 + independent_variable2, data = your_data_frame)

model3 <- lm(dependent_variable ~ independent_variable1 + independent_variable2, data = your_data_frame)

model4 <- lm(dependent_variable ~ independent_variable1 + independent_variable2, data = your_data_frame)

model5 <- lm(dependent_variable ~ independent_variable1 + independent_variable2, data = your_data_frame)

model6 <- lm(dependent_variable ~ independent_variable1 + independent_variable2, data = your_data_frame)

model7 <- lm(dependent_variable ~ independent_variable1 + independent_variable2, data = your_data_frame)

model8 <- lm(dependent_variable ~ independent_variable1 + independent_variable2, data = your_data_frame)

model9 <- lm(dependent_variable ~ independent_variable1 + independent_variable2, data = your_data_frame)



















# Create a regression results table using stargazer
stargazer(model1, model2, model3, model4, model5, model6, model7, model8,
          type = "text",  # for LaTeX output, set type to "latex"
          out = "regression_results_table.tex", # if you want to output to a .tex file
          title = "Table 1: Resultat av regressioner",
          covariate.labels = c("KVINNOR", "HANDEL", "INV", "GPT", "POP", "SKOLA", "FoU", "EU"), # Custom labels for covariates
          omit.stat = c("ll", "ser", "f"), # Omit statistics like log-likelihood, standard error of regression, F-statistic
          no.space = TRUE, # Remove extra space
          digits = 3, # Number of digits to display
          star.cutoffs = c(0.05, 0.01, 0.001), # Set cutoffs for stars
          align = TRUE # Align the columns
)

# Note: Replace model1, model2, ... with your actual model objects and adjust other parameters as needed.
