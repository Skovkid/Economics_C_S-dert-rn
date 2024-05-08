# Visualize missing data over time

library(dplyr)
library(tidyr)
library(purrr)
library(knitr)
library(ggplot2)


#Europe
sum_eur <- europe_data %>%
  summarise(
    across(
      .cols = where(is.numeric),  # This ensures that summary functions are only applied to numeric columns
      .fns = list(
        Obs = ~n(),
        Median = ~median(.x, na.rm = TRUE),
        Mean = ~mean(.x, na.rm = TRUE),
        SD = ~sd(.x, na.rm = TRUE),
        Min = ~min(.x, na.rm = TRUE),
        Max = ~max(.x, na.rm = TRUE)
      )
    )
  )

# Print the summary to check
print(sum_eur)

write.csv(sum_eur, "sum_eur.csv")


#Latin America 

sum_LM <- latin_america_data %>%
  summarise(
    across(
      .cols = where(is.numeric),  # This ensures that summary functions are only applied to numeric columns
      .fns = list(
        Obs = ~n(),
        Median = ~median(.x, na.rm = TRUE),
        Mean = ~mean(.x, na.rm = TRUE),
        SD = ~sd(.x, na.rm = TRUE),
        Min = ~min(.x, na.rm = TRUE),
        Max = ~max(.x, na.rm = TRUE)
      )
    )
  )

# Print the summary to check
print(sum_LM)

write.csv(sum_LM, "sum_LM.csv")

#US

sum_us <- us_canada_data %>%
  summarise(
    across(
      .cols = where(is.numeric),  # This ensures that summary functions are only applied to numeric columns
      .fns = list(
        Obs = ~n(),
        Median = ~median(.x, na.rm = TRUE),
        Mean = ~mean(.x, na.rm = TRUE),
        SD = ~sd(.x, na.rm = TRUE),
        Min = ~min(.x, na.rm = TRUE),
        Max = ~max(.x, na.rm = TRUE)
      )
    )
  )

# Print the summary to check
print(sum_us)

write.csv(sum_us, "sum_us.csv")



#Russia

sum_RU <- russia_central_asia_data %>%
  summarise(
    across(
      .cols = where(is.numeric),  # This ensures that summary functions are only applied to numeric columns
      .fns = list(
        Obs = ~n(),
        Median = ~median(.x, na.rm = TRUE),
        Mean = ~mean(.x, na.rm = TRUE),
        SD = ~sd(.x, na.rm = TRUE),
        Min = ~min(.x, na.rm = TRUE),
        Max = ~max(.x, na.rm = TRUE)
      )
    )
  )

# Print the summary to check
print(sum_RU)

write.csv(sum_RU, "sum_RU.csv")
################################################################################

# Create the scatter plot
ggplot(Reg_w_NA_df_No_TM, aes(x = Prop_Women, y = GDP_Per_Capita_Growth)) +
  geom_point(color = "blue", size = 2) +
  #ggtitle("Scatter Plot of GDP Per Capita Growth vs. Proportion Women in Parliament") +
  xlab("Proportion Women in Parliament") +
  ylab("GDP Per Capita Growth") +
  geom_hline(yintercept = 0, color = "red", size = 1.5) +  # Add horizontal line at y = 0
  theme_minimal()  # Applies a minimalistic theme



# Create the scatter plot
ggplot(Reg_w_NA_df_No_TM, aes(x = WBL_ind, y = Prop_Women)) +
  geom_point(color = "blue", size = 2) +
  #ggtitle("Women in Parliament and Legal Women Business and the Law") +
  xlab("WBL Index") +
  ylab("Proportion Women in Parliament") +
  theme_minimal()  # Applies a minimalistic theme




###################### TIME GRAPH #############################################

Line_EU <- europe_data %>%
  group_by(year) %>%
  summarize(MeanRepresentation = mean(Prop_Women),
            Mean_WBL = mean(WBL_ind),
            .groups = 'drop')


Line_LAM <- latin_america_data %>%
  group_by(year) %>%
  summarize(MeanRepresentation = mean(Prop_Women),
            Mean_WBL = mean(WBL_ind),
            .groups = 'drop')

Line_US <- us_canada_data %>%
  group_by(year) %>%
  summarize(MeanRepresentation = mean(Prop_Women),
            Mean_WBL = mean(WBL_ind),
            .groups = 'drop')

Line_RU <- russia_central_asia_data %>%
  group_by(year) %>%
  summarize(MeanRepresentation = mean(Prop_Women),
            Mean_WBL = mean(WBL_ind, na.rm=TRUE),
            .groups = 'drop')

#Parliamentary representation
ggplot() +
  geom_line(data = Line_EU, aes(x = year, y = MeanRepresentation, color = "Europe"), size = 1) +
  geom_line(data = Line_LAM, aes(x = year, y = MeanRepresentation, color = "Latin America"), size = 1) +
  geom_line(data = Line_US, aes(x = year, y = MeanRepresentation, color = "US and Canada"), size = 1) +
  geom_line(data = Line_RU, aes(x = year, y = MeanRepresentation, color = "Russia and Central Asia"), size = 1) +
  labs(title = "Mean Female Parliamentary Representation by Region",
       x = "Year",
       y = "Mean Female Representation (%)") +
  scale_color_manual(values = c("Europe" = "blue", "Latin America" = "green", "US and Canada" = "yellow", "Russia and Central Asia" = "red")) +
  theme_bw()


#WBL Index

ggplot() +
  geom_line(data = Line_EU, aes(x = year, y = Mean_WBL, color = "Europe"), size = 1) +
  geom_line(data = Line_LAM, aes(x = year, y = Mean_WBL, color = "Latin America"), size = 1) +
  geom_line(data = Line_US, aes(x = year, y = Mean_WBL, color = "US and Canada"), size = 1) +
  geom_line(data = Line_RU, aes(x = year, y = Mean_WBL, color = "Russia and Central Asia"), size = 1) +
  labs(title = "Mean WBL Index Score",
       x = "Year",
       y = "Mean WBL Score (1-100)") +
  scale_color_manual(values = c("Europe" = "blue", "Latin America" = "green", "US and Canada" = "yellow", "Russia and Central Asia" = "red")) +
  theme_bw()



