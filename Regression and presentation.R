install.packages("stargazer")

library(stargazer)


#Debugging format for regression:



# Assuming you have regression models stored in variables, for example:

model1 <- lm(result_latin_america$GDPCap ~ result_latin_america$PropWomen, data = result_latin_america)

 
model2 <- lm(dependent_variable ~ independent_variable1 + independent_variable2, data = your_data_frame)

model3 <- lm(dependent_variable ~ independent_variable1 + independent_variable2, data = your_data_frame)

model4 <- lm(dependent_variable ~ independent_variable1 + independent_variable2, data = your_data_frame)

model5 <- lm(dependent_variable ~ independent_variable1 + independent_variable2, data = your_data_frame)

model6 <- lm(dependent_variable ~ independent_variable1 + independent_variable2, data = your_data_frame)

model7 <- lm(dependent_variable ~ independent_variable1 + independent_variable2, data = your_data_frame)

model8 <- lm(dependent_variable ~ independent_variable1 + independent_variable2, data = your_data_frame)

model9 <- lm(dependent_variable ~ independent_variable1 + independent_variable2, data = your_data_frame)






summary(model1)












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
