library(tidyverse)
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)


# Create data frame
data <- read.csv("BD_Labs/Lab8/WB_Russia/a291dbd7-212a-4f56-9a66-ac2a85d293df_Data.csv")
print(data)
# Reshape data frame for further analysis
df <- df %>% pivot_longer(-c("Country.Name", "Country.Code", "Series.Name", "Series.Code"), names_to = "Year", values_to = "Value") %>% mutate(Year = parse_number(Year))

gdp_growth <- df %>% filter(Series.Name == "GDP growth (annual %)")

ggplot(gdp_growth, aes(x = Year, y = Value)) + geom_line() + geom_point() + labs(title = "GDP Growth Curve for Russia", x = "Year", y = "GDP Growth (%)")

population_growth <- df %>% filter(Series.Name == "Population growth (annual %)")

# Calculate dynamics
population_growth <- population_growth %>% mutate(Dynamics = (Value / lag(Value)) - 1)

ggplot(population_growth, aes(x = Year, y = Dynamics)) + geom_line() + geom_point() + labs(title = "Population Growth Dynamics for Russia", x = "Year", y = "Population Growth Dynamics")

# Life expectancy and mortality rate data
life_expectancy <- df %>% filter(Series.Name == "Life expectancy at birth, total (years)")
mortality_rate <- df %>% filter(Series.Name == "Mortality rate, under-5 (per 1,000 live births)")

# Merge data frames
merged_df <- life_expectancy %>% inner_join(mortality_rate, by = "Year", suffix = c("_life", "_mortality"))

# Correlation
cor(merged_df$Value_life, merged_df$Value_mortality, use = "complete.obs")

