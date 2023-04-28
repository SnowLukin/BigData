library(tidyverse)
library(scatterplot3d)
library(reshape2)
library(plotly)
library(cluster)
library(factoextra)
library(NbClust)
library(dendextend)
library(caret)
library(e1071)
library(rpart)
library(rpart.plot)
library(randomForest)
library(gridExtra)


data <- read.csv("BD_Labs/Lab6/student_mat.csv", header=TRUE, sep=",")

# Step 1: Descriptive analysis of the data
summary(data)
str(data)

ggplot(data, aes(x = G1)) +
  geom_histogram(color = "black", fill = "lightblue", bins = 20) +
  labs(title = "Histogram of G1", x = "Score", y = "Frequency") +
  theme_minimal()

ggplot(data, aes(x = G2)) +
  geom_histogram(color = "black", fill = "lightblue", bins = 20) +
  labs(title = "Histogram of G2", x = "Score", y = "Frequency") +
  theme_minimal()

ggplot(data, aes(x = G3)) +
  geom_histogram(color = "black", fill = "lightblue", bins = 20) +
  labs(title = "Histogram of G3", x = "Score", y = "Frequency") +
  theme_minimal()

ggplot(data, aes(x = absences)) +
  geom_histogram(color = "black", fill = "lightblue", bins = 20) +
  labs(title = "Histogram of absences", x = "Amount", y = "Frequency") +
  theme_minimal()

ggplot(data, aes(x = studytime)) +
  geom_histogram(color = "black", fill = "lightblue", bins = 20) +
  labs(title = "Histogram of studytime", x = "Score", y = "Frequency") +
  theme_minimal()

ggplot(data, aes(x = failures)) +
  geom_histogram(color = "black", fill = "lightblue", bins = 20) +
  labs(title = "Histogram of failures", x = "Amount", y = "Frequency") +
  theme_minimal()

ggplot(data, aes(x = Dalc)) +
  geom_histogram(color = "black", fill = "lightblue", bins = 20) +
  labs(title = "Histogram of Dalc", x = "Amount", y = "Frequency") +
  theme_minimal()

ggplot(data, aes(x = Walc)) +
  geom_histogram(color = "black", fill = "lightblue", bins = 20) +
  labs(title = "Histogram of Walc", x = "Amount", y = "Frequency") +
  theme_minimal()


#  Step 2: Hierarchical clustering and dendrogram
numeric_data <- data %>% select_if(is.numeric)

data_scaled <- scale(numeric_data)
# Perform hierarchical clustering
dist_matrix <- dist(data_scaled, method = "euclidean")

# Cut the tree to get the desired number of groups
k <- 3
hc <- hclust(dist_matrix, method = "ward.D2")
plot(hc, hang = -1)
rect.hclust(hc, k = k, border = "red")

groups <- cutree(hc, k)

# Calculate the average values for each group
numeric_data$Group <- as.factor(groups)
group_means <- aggregate(. ~ Group, numeric_data, mean)

# Reshape the data frame to a long format
group_means_long <- melt(group_means, id.vars = "Group")

# Create a bar plot
ggplot(group_means_long, aes(x = Group, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  xlab("Groups") +
  ylab("Average Value") +
  ggtitle("Average Values of Characteristics by Group") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#  Step 3: Build a diagram of the "Elbow method" (Stone scree)
fviz_nbclust(numeric_data, kmeans, method = "wss") + geom_vline(xintercept = k, linetype = 2)

# Step 4: Scatterplot using ggplot2 (k-means clustering):
ggplot(numeric_data, aes(x = G1, y = G2, color = Group)) +
  geom_point() +
  labs(title = "Scatterplot of G1 vs G2", x = "G1", y = "G2") +
  theme_minimal()

ggplot(numeric_data, aes(x = G1, y = G3, color = Group)) +
  geom_point() +
  labs(title = "Scatterplot of G1 vs G3", x = "G1", y = "G3") +
  theme_minimal()

ggplot(numeric_data, aes(x = G2, y = G3, color = Group)) +
  geom_point() +
  labs(title = "Scatterplot of G2 vs G3", x = "G2", y = "G3") +
  theme_minimal()

ggplot(numeric_data, aes(x = Dalc, y = G3, color = Group)) +
  geom_point() +
  labs(title = "Scatterplot of Dalc vs G3", x = "Dalc", y = "G3") +
  theme_minimal()

ggplot(numeric_data, aes(x = Walc, y = G3, color = Group)) +
  geom_point() +
  labs(title = "Scatterplot of Walc vs G3", x = "Walc", y = "G3") +
  theme_minimal()

ggplot(numeric_data, aes(x = absences, y = G3, color = Group)) +
  geom_point() +
  labs(title = "Scatterplot of absenses vs G3", x = "absenses", y = "G3") +
  theme_minimal()

ggplot(numeric_data, aes(x = studytime, y = G3, color = Group)) +
  geom_point() +
  labs(title = "Scatterplot of studytime vs G3", x = "studytime", y = "G3") +
  theme_minimal()

# Create a 3D scatterplot
colors <- as.integer(numeric_data$Group)
scatterplot3d(x = numeric_data$G1, y = numeric_data$G2, z = numeric_data$G3,
              color = colors, pch = 19,
              xlab = "G1", ylab = "G2", zlab = "G3",
              main = "3D Scatterplot of G1, G2, and G3")

# print amount of students in each group
print(table(groups))


# Lab 6.2
# Add the classes (groups) from the clustering analysis as a new column
data$Group <- as.factor(groups)

# Split the data into training and test sets
set.seed(123)
train_index <- createDataPartition(data$Group, p = 0.7, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# Train a naive Bayes classifier
nb_model <- naiveBayes(Group ~ ., data = train_data)

# Predict the test data using the naive Bayes classifier
test_data$predicted_group <- predict(nb_model, test_data)

# Calculate the accuracy of the predictions
confusion_matrix <- confusionMatrix(test_data$predicted_group, test_data$Group)
accuracy <- confusion_matrix$overall['Accuracy']
print(accuracy)

# Analyze the accuracy of the obtained solutions for the test data
print(confusion_matrix)

# 1. Apply the decision tree method to the classification problem
dt_model <- rpart(Group ~ ., data = train_data, method = "class")

# 2. Explore the decision tree and plot it, if dimensionality permits
print(dt_model)
print("-------------")
# 3. Analyze the accuracy of the obtained solutions for the test data
test_data$predicted_group_dt <- predict(dt_model, test_data, type = "class")
confusion_matrix_dt <- confusionMatrix(test_data$predicted_group_dt, test_data$Group)
accuracy_dt <- confusion_matrix_dt$overall['Accuracy']
print("-------------")
print(accuracy_dt)
print(confusion_matrix_dt)
print("-------------")

# 4. Perform classification using a random forest
rf_model <- randomForest(Group ~ ., data = train_data, importance = TRUE)
test_data$predicted_group_rf <- predict(rf_model, test_data)
confusion_matrix_rf <- confusionMatrix(test_data$predicted_group_rf, test_data$Group)
accuracy_rf <- confusion_matrix_rf$overall['Accuracy']
print(accuracy_rf)
print(confusion_matrix_rf)
print("-------------")

# 5. Compare the results with the results of the Bayesian classifier
cat("Naive Bayes Accuracy:", accuracy, "\n")
cat("Decision Tree Accuracy:", accuracy_dt, "\n")
cat("Random Forest Accuracy:", accuracy_rf, "\n")

# Plot the decision tree
rpart.plot(dt_model, type = 4, extra = 101, tweak = 1.2, main = "Decision Tree")

# Plot the variable importance for the random forest
rf_var_imp <- importance(rf_model)
rf_var_imp_df <- data.frame(Feature = rownames(rf_var_imp), Importance = rf_var_imp[, 1])
ggplot(rf_var_imp_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Random Forest Variable Importance", x = "Feature", y = "Importance") +
  theme_minimal() +
  coord_flip()
