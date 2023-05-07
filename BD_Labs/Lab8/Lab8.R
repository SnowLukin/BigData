library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(tidyverse)

# Read the dataset
data <- read.csv("BD_Labs/Lab8/WB_Russia/a291dbd7-212a-4f56-9a66-ac2a85d293df_Data.csv")

# Clean the data
data <- data %>%
  rename(year_1990 = `X1990.YR1990.`,
         year_2000 = `X2000.YR2000.`,
         year_2012 = `X2012.YR2012.`,
         year_2013 = `X2013.YR2013.`,
         year_2014 = `X2014.YR2014.`,
         year_2015 = `X2015.YR2015.`,
         year_2016 = `X2016.YR2016.`,
         year_2017 = `X2017.YR2017.`,
         year_2018 = `X2018.YR2018.`,
         year_2019 = `X2019.YR2019.`,
         year_2020 = `X2020.YR2020.`,
         year_2021 = `X2021.YR2021.`) %>%
  select(-Country.Code)

# Reshape the data
data_long <- data %>%
  pivot_longer(cols = starts_with("year"),
               names_to = "year",
               values_to = "value") %>%
  mutate(year = as.integer(str_remove(year, "year_")))

gdp_growth_data <- data_long %>%
  filter(Series.Name == "GDP growth (annual %)")

pop_growth_data <- data_long %>%
  filter(Series.Name == "Population growth (annual %)")

gdp_pop_growth <- inner_join(gdp_growth_data, pop_growth_data, by = c("Country.Name", "year")) %>%
  rename(gdp_growth = value.x,
         pop_growth = value.y)

gdp_pop_growth$gdp_growth <- as.numeric(gdp_pop_growth$gdp_growth)
gdp_pop_growth$pop_growth <- as.numeric(gdp_pop_growth$pop_growth)

gdp_pop_growth <- gdp_pop_growth %>%
  arrange(gdp_growth)

print(gdp_pop_growth$gdp_growth)
print(gdp_pop_growth$pop_growth)

# Plot GDP growth and population growth
ggplot(gdp_pop_growth, aes(x = year)) +
  geom_line(aes(y = gdp_growth, color = "GDP Growth", group = 1)) +
  geom_line(aes(y = pop_growth, color = "Population Growth", group = 1)) +
  labs(title = "GDP Growth and Population Growth in Russia",
       x = "Year",
       y = "Growth Rate (%)",
       color = "Indicator") +
  theme_minimal()

# Fit the linear regression model
gdp_growth_model <- lm(gdp_growth ~ year, data = gdp_pop_growth)

# Predict GDP growth for the next five years
future_years <- data.frame(year = (2022:2026))
gdp_growth_forecast <- predict(gdp_growth_model, future_years)

# Display the forecast
data.frame(year = future_years$year, gdp_growth_forecast = gdp_growth_forecast)
