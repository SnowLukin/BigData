# Determine the number of variables in the set using the command already known to you longley data;
ncol(longley)
# Use the command you already know to determine the sample size in a data set longley;
nrow(longley)

# Summarize simple statistics (descriptive analysis).
summary(longley)

# Use the cor(longley) function to get the correlation matrix and explain it.
cor_matrix <- cor(longley)
cor_matrix

# The correlation matrix shows the correlation coefficients between pairs of
# variables in the dataset. A high positive correlation coefficient (close to 1)
# indicates a strong positive linear relationship between the variables,
# while a high negative correlation coefficient (close to -1) indicates a strong
# negative linear relationship. A correlation coefficient close to 0 suggests
# a weak or no linear relationship between the variables.

# Support your explanations with graphs.
# To create a scatterplot matrix to visualize the relationships between variables, use the pairs() function:
pairs(longley)

# Check for normal distribution of variables from the longley dataset;
for (column in colnames(longley)) {
	print(paste("Shapiro-Wilk test for", column))
	print(shapiro.test(longley[[column]]))
}

# Substantiate the obtained results graphically.
# visualize the distribution of the variables
par(mfrow = c(2, 4))
for (column in colnames(longley)) {
	hist(longley[[column]], main = paste("Histogram for", column), xlab = column)
}

par(mfrow = c(2, 4))
for (column in colnames(longley)) {
	qqnorm(longley[[column]], main = paste("Q-Q plot of", column))
	qqline(longley[[column]])
}


# This code will display the Spearman correlation matrix for the longley dataset in the console,
# showing the strength and direction of the monotonic relationships between each pair of variables.
# The scatter plot matrix will visualize the relationships between the variables, with a linear regression
# line (in red) representing the best-fitting linear relationship for each pair of variables.

# Calculate the Spearman correlation matrix
cor_matrix <- cor(longley, method="spearman")
print(cor_matrix)

# Create scatterplot matrix with linear regression lines
pairs(longley, panel = function(x, y, ...){
  points(x, y, ...)
  abline(lm(y ~ x), col="red", lwd=2)
})





