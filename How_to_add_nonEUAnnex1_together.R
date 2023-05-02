library(dplyr)
library(purrr)

# create example data frames
df1 <- data.frame(country = c("USA", "USA", "Canada", "Canada"),
                  year = c(2010, 2010, 2010, 2011),
                  value1 = c(10, 20, 30, 40),
                  value2 = c(5, 10, 15, 20),
                  value3 = c(2, 4, 6, 8),
                  value4 = c(3, 6, 9, 12),
                  value5 = c(1, 2, 3, 4))

df2 <- data.frame(country = c("USA", "Canada", "Canada", "Mexico"),
                  year = c(2010, 2011, 2012, 2012),
                  value1 = c(5, 10, 15, 20),
                  value2 = c(2, 4, 6, 8),
                  value3 = c(1, 2, 3, 4),
                  value4 = c(3, 6, 9, 12),
                  value5 = c(1, 2, 3, 4))

df3 <- data.frame(country = c("Mexico", "Mexico", "Canada", "USA"),
                  year = c(2010, 2010, 2011, 2011),
                  value1 = c(15, 20, 25, 30),
                  value2 = c(6, 8, 10, 12),
                  value3 = c(3, 4, 5, 6),
                  value4 = c(9, 12, 15, 18),
                  value5 = c(3, 4, 5, 6))

df4 <- data.frame(country = c("USA", "USA", "Mexico", "Mexico"),
                  year = c(2011, 2012, 2010, 2011),
                  value1 = c(20, 25, 30, 35),
                  value2 = c(8, 10, 12, 14),
                  value3 = c(4, 5, 6, 7),
                  value4 = c(12, 15, 18, 21),
                  value5 = c(4, 5, 6, 7))

df5 <- data.frame(country = c("Mexico", "USA", "Canada", "Canada"),
                  year = c(2010, 2012, 2011, 2012),
                  value1 = c(25, 30, 35, 40),
                  value2 = c(10, 12, 14, 16),
                  value3 = c(5, 6, 7, 8),
                  value4 = c(15, 18, 21, 24),
                  value5 = c(5, 6, 7, 8))



# Combine the data frames into one
df_combined <- bind_rows(df1, df2, df3, df4, df5)

# Summarize the value columns
df_summarized <- df_combined %>%
  group_by(country, year) %>%
  summarize(across(starts_with("value"), sum)) %>%
  ungroup()

# Pivot the data frame to wide format
df_pivoted <- df_summarized %>%
  pivot_wider(names_from = "country", values_from = starts_with("value"))

# Add a new column for the total sum
df_pivoted$total_sum <- rowSums(df_pivoted[, -c(1, 2)])

# View the final pivoted data frame
df_pivoted

#-------

library(dplyr)
library(tidyr)

# Sample data frames
df1 <- data.frame(Year = c(2010, 2011, 2012, 2013), 
                  Country = c("USA", "Canada", "Germany", "France"), 
                  Exports = c(5, 10, 15, 20))


df2 <- data.frame(Year = c(2010, 2011, 2012, 2013), 
                  Country = c("USA", "Canada", "Germany", "France"), 
                  Exports = c(7, 21, 2, 14))

# Combine data frames and summarize total values
df_sum <- bind_rows(df1, df2) %>%
  pivot_longer(cols = starts_with("Exports"), 
               names_to = "Value", 
               values_to = "Total_Value") %>%
  group_by(Year, Country) %>%
  summarize(Total = sum(Total_Value), .groups = "drop") %>%
  select(Year, Country, Total)

# Output
df_sum

