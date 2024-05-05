# Visualize missing data over time

library(dplyr)
library(tidyr)
library(purrr)
library(knitr)
library(ggplot2)




observations_summary <- Reg_w_NA_df_No_TM %>%
  summarise(
    GDP_Growth_Cap_Observations = sum(!is.na(GDP_Growth_Cap)),
    GDP_Per_Capita_Growth_Observations = sum(!is.na(GDP_Per_Capita_Growth)),
    Prop_Women_Observations = sum(!is.na(Prop_Women)),
    Trade_Observations = sum(!is.na(Trade)),
    Capital_formation_Observations = sum(!is.na(Capital_formation)),
    WBL_ind_Observations = sum(!is.na(WBL_ind))
  )

# View the summary of observations
print(observations_summary)



# Create the scatter plot
ggplot(Reg_w_NA_df_No_TM, aes(x = Prop_Women, y = GDP_Per_Capita_Growth)) +
  geom_point(color = "blue", size = 2) +
  ggtitle("Scatter Plot of GDP Per Capita Growth vs. Proportion Women in Parliament") +
  xlab("Proportion Women in Parliament") +
  ylab("GDP Per Capita Growth") +
  geom_hline(yintercept = 0, color = "red", size = 1.5) +  # Add horizontal line at y = 0
  theme_minimal()  # Applies a minimalistic theme



# Create the scatter plot
ggplot(Reg_w_NA_df_No_TM, aes(x = WBL_ind, y = Prop_Women)) +
  geom_point(color = "blue", size = 2) +
  ggtitle("Women in Parliament and Legal Women Business and the Law") +
  xlab("WBL Index") +
  ylab("Proportion Women in Parliament") +
  theme_minimal()  # Applies a minimalistic theme
