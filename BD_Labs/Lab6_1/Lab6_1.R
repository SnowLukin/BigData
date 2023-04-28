library(tidyverse)
library(scatterplot3d)
library(reshape2)
library(plotly)
library(cluster)
library(factoextra)
library(NbClust)
library(dendextend)


data <- read.csv("BD_Labs/Lab6_1/student_mat.csv", header=TRUE, sep=",")

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

