# install.packages("ggplot2", repos = "https://cloud.r-project.org/")
# install.packages("dplyr", repos = "https://cloud.r-project.org/")
library(ggplot2)
library(farver)

lab2 <- function () {
  # Getting dataset
  travel_data <- read.csv("Lab2/travel_dataset.csv", header=TRUE, sep=";")

  # Select columns 3 to 12 of the DataFrame
  selected_columns <- travel_data[, 3:12]
  # print(selected_columns)

  min_values <- apply(selected_columns, 2, min, na.rm=TRUE)
  max_values <- apply(selected_columns, 2, max, na.rm=TRUE)
  mean_values <- apply(selected_columns, 2, mean, na.rm=TRUE)
  print("Min Values")
  print(min_values)
  print("Max Values")
  print(max_values)
  print("Mean Values")
  print(mean_values)
  # Count number of rows where mean value is greater than 7 out of 10
  moreThan7 <- sum(apply(selected_columns, 1, mean, na.rm=TRUE) > 0.7 * 10)
  # Count number of rows where mean value is less than 3 out of 10
  lessThan3 <- sum(apply(selected_columns, 1, mean, na.rm=TRUE) < 0.3 * 10)

  print(moreThan7)
  print(lessThan3)

  sorted_cols <- names(selected_columns)[order(-mean_values)]
  print(sorted_cols)

  # Build a bar chart
  df <- data.frame(work = sorted_cols, mean_rating=mean_values)
  ggplot(df, aes(x = work, y = mean_rating)) +
    geom_bar(stat = "identity") +
    labs(x = "Work", y = "Mean Rating")
}


lab2()
