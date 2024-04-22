install.packages("lmtest")
install.packages("sandwich")
install.packages("car")  # This is for diagnostic plots

# Load the packages
library(lmtest)
library(sandwich)
library(car)


model <- lm(GDP_Growth_Cap ~ Prop_women + Trade + Capital_formation + WBL_ind, data = Reg_w_NA_df_No_TM)

plot(fe_No_TM, which = 1)  # Residuals vs Fitted values plot
coeftest(fe_No_TM, vcov = vcovHC(fe_No_TM, type = "HC1"))

class(fe_No_TM)  # Check the class of fe_No_TM
summary(fe_No_TM)  # Get a summary to see if it's a fitted model

# Compute robust standard errors for panel models
robust_se <- vcovHC(fe_No_TM, method = "arellano", type = "HC1")


# Explicitly use vcovHC from plm package
robust_se <- plm::vcovHC(fe_No_TM, method = "arellano", type = "HC1")

# Apply coeftest with the robust standard errors
coeftest_result <- coeftest(fe_No_TM, vcov = robust_se)
print(coeftest_result)
