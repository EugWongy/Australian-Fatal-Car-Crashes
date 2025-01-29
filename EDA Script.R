# Title: Exploratory Data Analysis on the Australian Road Deaths Database- Fatal crashes by date
# Dataset: bitre_fatal_crashes_dec2024
# Dataset obtained from: Australian Road Deaths Database, Bureau of Infrastructure and Transport Research Economics (BITRE)

# Prepare data set
# Download "bitre_fatal_crashes_[date updated].xlsx" containing 4 tabs [Index, BITRE_Fatal_Crash, BITRE_Fatal_Crash_Count_By_Date, Appendix]
# Ensure dataset is readable by R
# - Row of titles is in Row 1 by deleting Headings and Notes in "BITRE_Fatal_Crash" & "BITRE_Fatal_Crash_Count_By_Date".
# - Replace NA values- BITRE uses -9 for missing/ unknown values. Use find all function in spreadsheet and replace all with "NA".


# Load required libraries -------------------------------------------------
library(tidyverse)
library(ggplot2)
library(dplyr)
library(readxl)

# Import and prepare dataset ----------------------------------------------

# Impport dataset downloaded straight from BITRE and assign the dataset to crash
crash <- read_xlsx(
  "~/Documents/R Studio/Australian Fatal Road Accidents/bitre_fatal_crashes_dec2024.xlsx",
  sheet = "BITRE_Fatal_Crash_Count_By_Date",
  skip = 2
)

# Check dataset & structure of variables
# Note: I would usually check for missing values, however this dataset does not have any missing values.

str(crash)
summary(crash)

# Factor Month and Day Of Week variables so that oberservations can be grouped and sorted later
crash_mutate <- crash %>%
  mutate(`Day Of Week` = factor(`Day Of Week`, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) %>%
  mutate(Month = factor(Month, levels = month.abb))

str(crash_mutate)

# Time series trend (Years) -----------------------------------------------

# Question: What does the data tell us about fatal road crashes in Australia over the years from 1989-2024?
crash_year <- crash_mutate %>%
  group_by(Year) %>%
  summarise(Total = sum(`Number of fatal crashes`))

summary(crash_year)

ggplot(crash_year, aes(x = Year, y = Total)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_smooth() +
  labs(
    title = "Fatal crashes in Australia, 1989-2024",
    x = "Year",
    y = "Number of fatal crashes",
    caption = "Data source: Bureau of Infrastructure and Transport Research Economics (BITRE)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))


# Time series trend (Months) ----------------------------------------------

# Question: Is there a month with higher or lower number of fatal crashes?

crash_month <- crash_mutate %>%
  group_by(Month) %>%
  summarise(Total = sum(`Number of fatal crashes`)) %>%
  mutate(Month = factor(Month, levels = month.abb)) %>%
  arrange(Month)

summary(crash_month)

ggplot(crash_month, aes(x = Month, y = Total)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = Total), vjust = -0.5) +
  labs(
    title = "Fatal crashes per month in Australia, 1989-2024",
    x = "Month",
    y = "Number of fatal crashes",
    caption = "Data source: Bureau of Infrastructure and Transport Research Economics (BITRE)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))


# Time series trend (Day of the week) -------------------------------------

# Question: Is there a specific day with higher or lower fatal crashes?

crash_day <- crash %>%
  group_by(`Day Of Week`) %>%
  summarise(Total = sum(`Number of fatal crashes`)) %>%
  mutate(`Day Of Week` = factor(`Day Of Week`, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")))

summary(crash_day)

ggplot(crash_day, aes(x = `Day Of Week`, y = Total)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(
    title = "Number of fatal crashes grouped by Day in Australia, 1989-2024",
    x = "Day of the week",
    y = "Total Number of fatal crashes",
    caption = "Data source: Bureau of Infrastructure and Transport Research Economics (BITRE)"
  ) +
  geom_text(aes(label = Total), vjust = -0.5) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))