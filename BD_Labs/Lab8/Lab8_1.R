library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(tidyverse)
library(ellipse)
library(corrplot)


# Read the dataset
data <- read.csv("BD_Labs/Lab8/wb_georgia.csv")

# Clean the data
data <- data %>%
	rename(
		year_1989 = `X1989..YR1989.`,
		year_1990 = `X1990..YR1990.`,
		year_1991 = `X1991..YR1991.`,
		year_1992 = `X1992..YR1992.`,
		year_1993 = `X1993..YR1993.`,
		year_1994 = `X1994..YR1994.`,
		year_1995 = `X1995..YR1995.`,
		year_1996 = `X1996..YR1996.`,
		year_1997 = `X1997..YR1997.`,
		year_1998 = `X1998..YR1998.`,
		year_1999 = `X1999..YR1999.`,
		year_2000 = `X2000..YR2000.`,
		year_2012 = `X2012..YR2012.`,
		year_2013 = `X2013..YR2013.`,
		year_2014 = `X2014..YR2014.`,
		year_2015 = `X2015..YR2015.`,
		year_2016 = `X2016..YR2016.`,
		year_2017 = `X2017..YR2017.`,
		year_2018 = `X2018..YR2018.`) %>%
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

# Plot GDP growth and population growth
ggplot(gdp_pop_growth, aes(x = year)) +
	geom_line(aes(y = gdp_growth, color = "GDP Growth", group = 1)) +
	geom_line(aes(y = pop_growth, color = "Population Growth", group = 1)) +
	labs(title = "GDP Growth and Population Growth in Georgia",
		 x = "Year",
		 y = "Growth Rate (%)",
		 color = "Indicator") +
	theme_minimal()

# Calculate the correlation coefficient
correlation_coefficient <- cor(gdp_pop_growth$gdp_growth, gdp_pop_growth$pop_growth, use = "complete.obs")

# Create a scatterplot of unemployment rate vs. population growth
ggplot(gdp_pop_growth, aes(x = gdp_growth, y = pop_growth)) +
    geom_point() +
    labs(title = paste("Scatterplot: GDP Growth vs. Population Growth\nCorrelation Coefficient:", round(correlation_coefficient, 2)),
         x = "GDP Growth (%)",
         y = "Population Growth (%)") +
    theme_minimal()

# Calculate the correlation matrix
correlation_matrix <- cor(gdp_pop_growth[, c("gdp_growth", "pop_growth")], use = "complete.obs")

# Plot the correlation matrix using plotcorr()
plotcorr(correlation_matrix, mar = c(1, 1, 1, 1))

unemployment_data <- data_long %>%
	filter(Series.Name == "Unemployment with basic education (% of total labor force with basic education)")

pop_growth_data <- data_long %>%
	filter(Series.Name == "Population growth (annual %)")

# Join unemployment data with population growth data
unemployment_pop_growth <- inner_join(unemployment_data, pop_growth_data, by = c("Country.Name", "year")) %>%
	rename(unemployment_rate = value.x,
		   pop_growth = value.y)

unemployment_pop_growth$unemployment_rate <- as.numeric(unemployment_pop_growth$unemployment_rate)
unemployment_pop_growth$pop_growth <- as.numeric(unemployment_pop_growth$pop_growth)

unemployment_pop_growth <- unemployment_pop_growth %>%
	arrange(unemployment_rate)

# Plot unemployment rate and population growth
ggplot(unemployment_pop_growth, aes(x = year)) +
	geom_line(aes(y = unemployment_rate, color = "Unemployment Rate", group = 1)) +
	geom_line(aes(y = pop_growth, color = "Population Growth", group = 1)) +
	labs(title = "Unemployment Rate and Population Growth in Georgia",
		 x = "Year",
		 y = "Rate (%)",
		 color = "Indicator") +
	theme_minimal()

# Calculate the correlation coefficient
correlation_coefficient <- cor(unemployment_pop_growth$unemployment_rate, unemployment_pop_growth$pop_growth, use = "complete.obs")

# Create a scatterplot of unemployment rate vs. population growth
ggplot(unemployment_pop_growth, aes(x = unemployment_rate, y = pop_growth)) +
    geom_point() +
    labs(title = paste("Scatterplot: Unemployment Rate vs. Population Growth\nCorrelation Coefficient:", round(correlation_coefficient, 2)),
         x = "Unemployment Rate (%)",
         y = "Population Growth (%)") +
    theme_minimal()

# Calculate the correlation matrix
correlation_matrix <- cor(unemployment_pop_growth[, c("unemployment_rate", "pop_growth")], use = "complete.obs")

# Plot the correlation matrix using plotcorr()
plotcorr(correlation_matrix, mar = c(1, 1, 1, 1))


################
death_rate_data <- data_long %>%
	filter(Series.Name == "Death rate, crude (per 1,000 people)")

life_expectancy_data <- data_long %>%
	filter(Series.Name == "Life expectancy at birth, total (years)")

health_exp_data <- data_long %>%
	filter(Series.Name == "Domestic general government health expenditure per capita (current US$)")

# Join death rate data with life expectancy data
death_rate_life_expectancy <- inner_join(death_rate_data, life_expectancy_data, by = c("Country.Name", "year")) %>%
  rename(death_rate = value.x,
         life_expectancy = value.y)

death_rate_life_expectancy$death_rate <- as.numeric(death_rate_life_expectancy$death_rate)
death_rate_life_expectancy$life_expectancy <- as.numeric(death_rate_life_expectancy$life_expectancy)

death_rate_life_expectancy <- death_rate_life_expectancy %>%
  arrange(death_rate)

# Plot death rate and life expectancy
ggplot(death_rate_life_expectancy, aes(x = year)) +
  geom_line(aes(y = death_rate, color = "Death Rate", group = 1)) +
  geom_line(aes(y = life_expectancy, color = "Life Expectancy", group = 1)) +
  labs(title = "Death Rate and Life Expectancy in Georgia",
       x = "Year",
       y = "Rate/Expectancy",
       color = "Indicator") +
  theme_minimal()

# Calculate the correlation coefficient
correlation_coefficient <- cor(death_rate_life_expectancy$death_rate, death_rate_life_expectancy$life_expectancy, use = "complete.obs")

# Create a scatterplot of death rate vs. life expectancy
ggplot(death_rate_life_expectancy, aes(x = death_rate, y = life_expectancy)) +
  geom_point() +
  labs(title = paste("Scatterplot: Death Rate vs. Life Expectancy\nCorrelation Coefficient:", round(correlation_coefficient, 2)),
       x = "Death Rate (per 1,000 people)",
       y = "Life Expectancy (years)") +
  theme_minimal()

# Calculate the correlation matrix
correlation_matrix <- cor(death_rate_life_expectancy[, c("death_rate", "life_expectancy")], use = "complete.obs")

# Plot the correlation matrix using plotcorr()
plotcorr(correlation_matrix, mar = c(1, 1, 1, 1))

# Join health expenditure data with life expectancy data
health_exp_life_expectancy <- inner_join(health_exp_data, life_expectancy_data, by = c("Country.Name", "year")) %>%
  rename(health_expenditure = value.x,
         life_expectancy = value.y)

health_exp_life_expectancy$health_expenditure <- as.numeric(health_exp_life_expectancy$health_expenditure)
health_exp_life_expectancy$life_expectancy <- as.numeric(health_exp_life_expectancy$life_expectancy)

health_exp_life_expectancy <- health_exp_life_expectancy %>%
  arrange(health_expenditure)

# Plot health expenditure and life expectancy
ggplot(health_exp_life_expectancy, aes(x = year)) +
  geom_line(aes(y = health_expenditure, color = "Health Expenditure", group = 1)) +
  geom_line(aes(y = life_expectancy, color = "Life Expectancy", group = 1)) +
  labs(title = "Health Expenditure and Life Expectancy in Georgia",
       x = "Year",
       y = "Expenditure/Expectancy",
       color = "Indicator") +
  theme_minimal()

# Calculate the correlation coefficient
correlation_coefficient <- cor(health_exp_life_expectancy$health_expenditure, health_exp_life_expectancy$life_expectancy, use = "complete.obs")

# Create a scatterplot of health expenditure vs. life expectancy
ggplot(health_exp_life_expectancy, aes(x = health_expenditure, y = life_expectancy)) +
  geom_point() +
  labs(title = paste("Scatterplot: Health Expenditure vs. Life Expectancy\nCorrelation Coefficient:", round(correlation_coefficient, 2)),
       x = "Health Expenditure (USD)",
       y = "Life Expectancy (years)") +
  theme_minimal()

# Calculate the correlation matrix
correlation_matrix <- cor(health_exp_life_expectancy[, c("health_expenditure", "life_expectancy")], use = "complete.obs")

# Plot the correlation matrix using plotcorr()
plotcorr(correlation_matrix, mar = c(1, 1, 1, 1))

###############
uni_edu <- data_long %>%
	filter(Series.Name == "Educational attainment, at least Bachelor's or equivalent, population 25+, total (%) (cumulative)")

export <- data_long %>%
	filter(Series.Name == "Exports of goods and services (annual % growth)")

tech_ind <- data_long %>%
	filter(Series.Name == "Medium and high-tech Industry (including construction) (% manufacturing value added)")

# Join educational attainment data with exports data
edu_exports <- inner_join(uni_edu, export, by = c("Country.Name", "year")) %>%
  rename(educational_attainment = value.x,
         exports_growth = value.y)

edu_exports$educational_attainment <- as.numeric(edu_exports$educational_attainment)
edu_exports$exports_growth <- as.numeric(edu_exports$exports_growth)

edu_exports <- edu_exports %>%
  arrange(educational_attainment)

# Plot educational attainment and exports growth
ggplot(edu_exports, aes(x = year)) +
  geom_line(aes(y = educational_attainment, color = "Educational Attainment", group = 1)) +
  geom_line(aes(y = exports_growth, color = "Exports Growth", group = 1)) +
  labs(title = "Educational Attainment and Exports Growth in Georgia",
       x = "Year",
       y = "Percentage",
       color = "Indicator") +
  theme_minimal()

# Calculate the correlation coefficient
correlation_coefficient <- cor(edu_exports$educational_attainment, edu_exports$exports_growth, use = "pairwise.complete.obs")

# Create a scatterplot of educational attainment vs. exports growth
ggplot(edu_exports, aes(x = educational_attainment, y = exports_growth)) +
  geom_point() +
  labs(title = paste("Scatterplot: Educational Attainment vs. Exports Growth\nCorrelation Coefficient:", round(correlation_coefficient, 2)),
       x = "Educational Attainment (%)",
       y = "Exports Growth (%)") +
  theme_minimal()

# Calculate the correlation matrix
correlation_matrix <- cor(edu_exports[, c("educational_attainment", "exports_growth")], use = "pairwise.complete.obs")

# Plot the correlation matrix using plotcorr()
plotcorr(correlation_matrix, mar = c(1, 1, 1, 1))

# Join educational attainment data with tech industry data
edu_tech_ind <- inner_join(uni_edu, tech_ind, by = c("Country.Name", "year")) %>%
  rename(education_attainment = value.x,
         tech_industry = value.y)

edu_tech_ind$education_attainment <- as.numeric(edu_tech_ind$education_attainment)
edu_tech_ind$tech_industry <- as.numeric(edu_tech_ind$tech_industry)

edu_tech_ind <- edu_tech_ind %>%
  arrange(education_attainment)

# Plot educational attainment and tech industry
ggplot(edu_tech_ind, aes(x = year)) +
  geom_line(aes(y = education_attainment, color = "Educational Attainment", group = 1)) +
  geom_line(aes(y = tech_industry, color = "Tech Industry", group = 1)) +
  labs(title = "Educational Attainment and Tech Industry in Georgia",
       x = "Year",
       y = "Percentage",
       color = "Indicator") +
  theme_minimal()

# Calculate the correlation coefficient
correlation_coefficient <- cor(edu_tech_ind$education_attainment, edu_tech_ind$tech_industry, use = "pairwise.complete.obs")

# Create a scatterplot of educational attainment vs. tech industry
ggplot(edu_tech_ind, aes(x = education_attainment, y = tech_industry)) +
  geom_point() +
  labs(title = paste("Scatterplot: Educational Attainment vs. Tech Industry\nCorrelation Coefficient:", round(correlation_coefficient, 2)),
       x = "Educational Attainment (%)",
       y = "Tech Industry (%)") +
  theme_minimal()

# Calculate the correlation matrix
correlation_matrix <- cor(edu_tech_ind[, c("education_attainment", "tech_industry")], use = "pairwise.complete.obs")

# Plot the correlation matrix using plotcorr()
plotcorr(correlation_matrix, mar = c(1, 1, 1, 1))


###########
uni_edu_women <- data_long %>%
	filter(Series.Name == "Educational attainment, at least Bachelor's or equivalent, population 25+, female (%) (cumulative)")

edu_exp <- data_long %>%
	filter(Series.Name == "Government expenditure on education, total (% of GDP)")


# Join educational attainment data with education expenditure data
uni_edu_women_edu_exp <- inner_join(uni_edu_women, edu_exp, by = c("Country.Name", "year")) %>%
  rename(uni_edu_women = value.x,
         edu_expenditure = value.y)

uni_edu_women_edu_exp$uni_edu_women <- as.numeric(uni_edu_women_edu_exp$uni_edu_women)
uni_edu_women_edu_exp$edu_expenditure <- as.numeric(uni_edu_women_edu_exp$edu_expenditure)

uni_edu_women_edu_exp <- uni_edu_women_edu_exp %>%
  arrange(uni_edu_women)

# Plot educational attainment for women and education expenditure
ggplot(uni_edu_women_edu_exp, aes(x = year)) +
  geom_line(aes(y = uni_edu_women, color = "Educational Attainment for Women", group = 1)) +
  geom_line(aes(y = edu_expenditure, color = "Education Expenditure (% of GDP)", group = 1)) +
  labs(title = "Educational Attainment for Women and Education Expenditure in Georgia",
       x = "Year",
       y = "Percentage",
       color = "Indicator") +
  theme_minimal()

# Calculate the correlation coefficient
correlation_coefficient <- cor(uni_edu_women_edu_exp$uni_edu_women, uni_edu_women_edu_exp$edu_expenditure, use = "pairwise.complete.obs")

# Create a scatterplot of educational attainment for women vs. education expenditure
ggplot(uni_edu_women_edu_exp, aes(x = uni_edu_women, y = edu_expenditure)) +
  geom_point() +
  labs(title = paste("Scatterplot: Educational Attainment for Women vs. Education Expenditure\nCorrelation Coefficient:", round(correlation_coefficient, 2)),
       x = "Educational Attainment for Women (%)",
       y = "Education Expenditure (% of GDP)") +
  theme_minimal()

# Calculate the correlation matrix
correlation_matrix <- cor(uni_edu_women_edu_exp[, c("uni_edu_women", "edu_expenditure")], use = "pairwise.complete.obs")

# Plot the correlation matrix using plotcorr()
plotcorr(correlation_matrix, mar = c(1, 1, 1, 1))


###########
uni_edu <- data_long %>%
	filter(Series.Name == "Educational attainment, at least Bachelor's or equivalent, population 25+, total (%) (cumulative)")

articles <- data_long %>%
	filter(Series.Name == "Scientific and technical journal articles")

# Join university education data with articles data
uni_edu_articles <- inner_join(uni_edu, articles, by = c("Country.Name", "year")) %>%
  rename(education_attainment = value.x,
         journal_articles = value.y)

uni_edu_articles$education_attainment <- as.numeric(uni_edu_articles$education_attainment)
uni_edu_articles$journal_articles <- as.numeric(uni_edu_articles$journal_articles)

uni_edu_articles <- uni_edu_articles %>%
  arrange(education_attainment)

# Plot educational attainment and journal articles
ggplot(uni_edu_articles, aes(x = year)) +
  geom_line(aes(y = education_attainment, color = "Education Attainment", group = 1)) +
  geom_line(aes(y = journal_articles, color = "Journal Articles", group = 1)) +
  labs(title = "Educational Attainment and Journal Articles in Georgia",
       x = "Year",
       y = "Percentage/Count",
       color = "Indicator") +
  theme_minimal()

# Calculate the correlation coefficient
correlation_coefficient <- cor(uni_edu_articles$education_attainment, uni_edu_articles$journal_articles, use = "pairwise.complete.obs")

# Create a scatterplot of educational attainment vs. journal articles
ggplot(uni_edu_articles, aes(x = education_attainment, y = journal_articles)) +
  geom_point() +
  labs(title = paste("Scatterplot: Educational Attainment vs. Journal Articles\nCorrelation Coefficient:", round(correlation_coefficient, 2)),
       x = "Educational Attainment (%)",
       y = "Journal Articles (Count)") +
  theme_minimal()

# Calculate the correlation matrix
correlation_matrix <- cor(uni_edu_articles[, c("education_attainment", "journal_articles")], use = "pairwise.complete.obs")

# Plot the correlation matrix using plotcorr()
plotcorr(correlation_matrix, mar = c(1, 1, 1, 1))


# Fit the linear regression model
gdp_growth_model <- lm(gdp_growth ~ year, data = gdp_pop_growth)

# Predict GDP growth for the next five years
future_years <- data.frame(year = (2024:2030))
gdp_growth_forecast <- predict(gdp_growth_model, future_years)

# Display the forecast
data.frame(year = future_years$year, gdp_growth_forecast = gdp_growth_forecast)
