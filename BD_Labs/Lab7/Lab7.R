library(ggplot2)
library(dplyr)

# Read the dataset
athlen_events <- read.csv("BD_Labs/Lab7/athlete_events.csv")

# Filter data for Russian weightlifting athletes
russian_weightlifting <- athlen_events %>%
  filter(NOC == "RUS", Sport == "Weightlifting")

# Hypothesis testing for average weight of Russian weightlifting athletes
hypothesis_test <- wilcox.test(russian_weightlifting$Weight, mu = 80)

# Print the result
print(hypothesis_test)

# Filter data by gender and sport
men_data <- athlen_events %>% filter(Sex == "M")
women_data <- athlen_events %>% filter(Sex == "F")

# Choose two sports for comparison, e.g. "Basketball" and "Volleyball"
sport1 <- "Basketball"
sport2 <- "Volleyball"

men_sport1 <- men_data %>% filter(Sport == sport1)
men_sport2 <- men_data %>% filter(Sport == sport2)

women_sport1 <- women_data %>% filter(Sport == sport1)
women_sport2 <- women_data %>% filter(Sport == sport2)

# Histograms and smoothed histograms for men of the first sport
ggplot(men_sport1, aes(x = Weight)) +
  geom_histogram(binwidth = 5, fill = "blue", alpha = 0.5) +
  geom_density(aes(y = ..count..), color = "red", linetype = "dashed")

# Histograms and smoothed histograms for men of the second sport
ggplot(men_sport2, aes(x = Weight)) +
  geom_histogram(binwidth = 5, fill = "blue", alpha = 0.5) +
  geom_density(aes(y = ..count..), color = "red", linetype = "dashed")

ggplot(women_sport1, aes(x = Weight)) +
  geom_histogram(binwidth = 5, fill = "blue", alpha = 0.5) +
  geom_density(aes(y = ..count..), color = "red", linetype = "dashed")

# Histograms and smoothed histograms for men of the second sport
ggplot(women_sport2, aes(x = Weight)) +
  geom_histogram(binwidth = 5, fill = "blue", alpha = 0.5) +
  geom_density(aes(y = ..count..), color = "red", linetype = "dashed")

# Shapiro test for normality
shapiro_test_men_sport1 <- shapiro.test(men_sport1$Weight)
shapiro_test_men_sport2 <- shapiro.test(men_sport2$Weight)

shapiro_test_women_sport1 <- shapiro.test(women_sport1$Weight)
shapiro_test_women_sport2 <- shapiro.test(women_sport2$Weight)

# QQ Plots
par(mfrow = c(2, 2))

qqnorm(men_sport1$Weight, main = paste("M basketball", sport1))
qqline(men_sport1$Weight)

qqnorm(men_sport2$Weight, main = paste("М val", sport2))
qqline(men_sport2$Weight)

qqnorm(women_sport1$Weight, main = paste("W basketball", sport1))
qqline(women_sport1$Weight)

qqnorm(women_sport2$Weight, main = paste("W val", sport2))
qqline(women_sport2$Weight)

cat("Shapiro test results:\n")
cat("----------------------\n")
cat(paste("Men in", sport1, ":\n"))
print(shapiro_test_men_sport1)
cat(paste("\nMen in", sport2, ":\n"))
print(shapiro_test_men_sport2)
cat(paste("\nWomen in", sport1, ":\n"))
print(shapiro_test_women_sport1)
cat(paste("\nWomen in", sport2, ":\n"))
print(shapiro_test_women_sport2)


# Perform the Wilcoxon rank-sum test for men
wilcoxon_test_men <- wilcox.test(Weight ~ Sport, data = rbind(men_sport1, men_sport2))

# Print the results
print(wilcoxon_test_men)

# Perform the Wilcoxon rank-sum test for women
wilcoxon_test_women <- wilcox.test(Weight ~ Sport, data = rbind(women_sport1, women_sport2))

# Print the results
print(wilcoxon_test_women)

cat("Conclusions:\n")
cat("----------------\n")
cat("Hypothesis test for average weight of Russian weightlifting athletes:\n")
print(hypothesis_test)

cat("\nHypothesis test for equality of average weight of men in", sport1, "and", sport2, ":\n")
print(wilcoxon_test_men)

cat("\nHypothesis test for equality of average weight of women in", sport1, "and", sport2, ":\n")
print(wilcoxon_test_women)
