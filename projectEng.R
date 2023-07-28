# Load data containing information about students' food choices.
# The survey was conducted on 125 students.
data <- read.csv("D:/Michal/Nauka/Studia/Rok 3/Semestr 6/MUMa/Lab/Projekt/food_coded.csv")

# Loading the library for creating plots
library(ggplot2)

#---------------------------------------------------------------------------------
# Bar chart showing the number of students in each cuisine group they grew up with

# Create a new variable 'cuisine_factor' by converting the 'cuisine' variable to a factor
data$cuisine <- factor(data$cuisine)

ggplot(data, aes(x = cuisine)) +
  geom_bar() +
  labs(x = "Cuisine Style", y = "Number of Students") +
  ggtitle("Bar Chart - Cuisine Style") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  scale_x_discrete(labels = c("1 - American", "2 - Mexican/Spanish", "3 - Korean/Asian",
                              "4 - Indian", "5 - American inspired international dishes",
                              "6 - Other")) +
  scale_fill_manual(values = c("1 - American" = "red", "2 - Mexican/Spanish" = "blue",
                               "3 - Korean/Asian" = "green", "4 - Indian" = "purple",
                               "5 - American inspired international dishes" = "orange",
                               "6 - Other" = "gray"),
                    guide = guide_legend(title = "Legend"))

#---------------------------------------------------------------------------------
# Pie chart showing the number of students in each group of preferred cuisines

# Convert the 'fav_cuisine_coded' variable to a factor
data$fav_cuisine_coded <- factor(data$fav_cuisine_coded, levels=c(0:8), 
                                 labels=c("0", "1", "2", "3", "4", "5", "6", "7", "8"))

# Create a vector containing labels for each category, which will be displayed in the legend
legend_labels <- c("None", "Italian/French/Greek", "Spanish/Mexican", "Arabic/Turkish",
                   "Asian/Chinese/Thai/Nepal", "American", "African", "Jamaican", "Indian")

# Create a vector containing colors for each category
legend_colors <- c("#E41A1C", "#377EB8", "#4DAF4A", "#000EA3", 
                   "#FF7F00", "#FFFF33", "#A65628", "#9780BF", "#999999")

ggplot(data, aes(x = "", fill = fav_cuisine_coded)) +
  geom_bar(width = 1) +
  labs(fill = "Preferred Cuisine") +
  ggtitle("Pie Chart - Preferred Cuisine") +
  theme_void() +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = legend_colors, labels = legend_labels)

#---------------------------------------------------------------------------------
# Scatter plot showing the current diet and perceived healthiness

# Create a vector containing labels for each category, which will be displayed on the x-axis
x_labels <- c("healthy/balanced/moderated", "unhealthy/cheap/too much/random", 
              "the same thing over and over", "unclear")

ggplot(data, aes(x = factor(diet_current_coded), y = healthy_feeling)) +
  geom_point(alpha = 0.1) +
  labs(x = "Current Diet", y = "Perceived Healthiness") +
  ggtitle("Violin Plot - Comparison of Ideal Diet with Current Diet") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(labels = x_labels)

#---------------------------------------------------------------------------------
# Predicting the favorite cuisine using Decision Trees
#---------------------------------------------------------------------------------
# Load required libraries
library(rpart)
library(dplyr)

# Load data into a corrected variable
data_corrected <- data

# Remove unnecessary columns that contain responses to open-ended questions
data_corrected <- select(data_corrected, -comfort_food, -comfort_food_reasons,
                         -diet_current, -eating_changes, -father_profession,
                         -fav_cuisine, -healthy_meal, -meals_dinner_friend,
                         -mother_profession, -type_sports, -food_childhood, -ideal_diet)

# Remove columns related to calories estimation since they are not needed
data_corrected <- select(data_corrected, -calories_chicken, -calories_scone,
                         -tortilla_calories, -turkey_calories, -waffle_calories)

# Convert columns to numeric type
data_corrected$weight <- as.numeric(data_corrected$weight)
data_corrected$GPA <- as.numeric(data_corrected$GPA)

# Split data into training and testing sets (e.g., 90% training, 10% testing)
set.seed(123)  # Set the random seed for reproducibility
train_indices <- sample(1:nrow(data_corrected), 0.9 * nrow(data_corrected))  # Training sample indices
train_data <- data_corrected[train_indices, ]  # Training data
test_data <- data_corrected[-train_indices, ]  # Testing data

# Create a decision tree model
ctrl <- rpart.control(cp = 0.0001)  # Control parameter for decision tree pruning
model <- rpart(fav_cuisine_coded ~ ., data = train_data, control = ctrl)

# Make predictions on the test set
predictions <- predict(model, newdata = test_data, type = "class")

# Evaluate the model's accuracy
accuracy <- sum(predictions == test_data$fav_cuisine_coded) / nrow(test_data)
cat("Decision Tree Model Accuracy:", accuracy)

# Combine predicted and actual values into a data frame
result <- data.frame(predictions, test_data$fav_cuisine_coded)

# Print the comparison of predicted and actual values
print(result)

# Create a plot to compare predicted and actual values
result$Index <- rownames(result)
result$Index <- as.numeric(result$Index)

ggplot(result, aes(x = Index)) +
  geom_point(aes(y = predictions), color = "blue", size = 3, shape = 3) +
  geom_point(aes(y = test_data$fav_cuisine_coded), color = "red", size = 3, shape = 4) +
  labs(x = "Index", y = "Value") +
  ggtitle("Comparison of Predictions and Real Data") +
  scale_x_continuous(breaks = result$Index)

# Confusion Matrix
library(caret)
confusionMatrix(result$predictions, result$test_data.fav_cuisine_coded)

#-------------------------------------------------------------------------------------------------------
# Predicting the favorite cuisine using Random Forest
#-------------------------------------------------------------------------------------------------------
# Load required libraries
library(randomForest)

# Load data into a corrected variable
data_corrected <- data

# Remove unnecessary columns that contain responses to open-ended questions
data_corrected <- select(data_corrected, -comfort_food, -comfort_food_reasons,
                         -diet_current, -eating_changes, -father_profession,
                         -fav_cuisine, -healthy_meal, -meals_dinner_friend,
                         -mother_profession, -type_sports, -food_childhood, -ideal_diet)

# Remove columns related to calories estimation since they are not needed
data_corrected <- select(data_corrected, -calories_chicken, -calories_scone,
                         -tortilla_calories, -turkey_calories, -waffle_calories)

# Convert columns to numeric type
data_corrected$weight <- as.numeric(data_corrected$weight)
data_corrected$GPA <- as.numeric(data_corrected$GPA)

# Remove columns with a lot of missing data to clean data for the random forest model
# Calculate the count of missing data
nan_counts <- colSums(is.na(data_corrected))

# Sort the counts
sorted_nan_counts <- sort(nan_counts, decreasing = TRUE)

# Print which columns contain how many missing data
for (column in names(sorted_nan_counts)) {
  cat("Column:", column, "\tNaN Count:", sorted_nan_counts[column], "\n")
}

# Remove "calories_day" column
data_corrected <- data_corrected[, -which(names(data_corrected) == "calories_day")]
# Remove "comfort_food_reasons_coded" column
data_corrected <- data_corrected[, -which(names(data_corrected) == "comfort_food_reasons_coded")]
# Remove "cuisine" column
data_corrected <- data_corrected[, -which(names(data_corrected) == "cuisine")]
# Remove "exercise" column
data_corrected <- data_corrected[, -which(names(data_corrected) == "exercise")]
# Remove "employment" column
data_corrected <- data_corrected[, -which(names(data_corrected) == "employment")]
# Remove "GPA" column
data_corrected <- data_corrected[, -which(names(data_corrected) == "GPA")]
# Remove "weight" column
data_corrected <- data_corrected[, -which(names(data_corrected) == "weight")]
# Remove "cook" column
data_corrected <- data_corrected[, -which(names(data_corrected) == "cook")]
# Remove "mother_education" column
data_corrected <- data_corrected[, -which(names(data_corrected) == "mother_education")]
# Remove "drink" column
data_corrected <- data_corrected[, -which(names(data_corrected) == "drink")]
# Remove "sports" column
data_corrected <- data_corrected[, -which(names(data_corrected) == "sports")]

# Remove rows with missing data
data_corrected <- na.omit(data_corrected)

# Split data into training and testing sets
set.seed(123)
train_indices <- sample(1:nrow(data_corrected), 0.9 * nrow(data_corrected))
train_data <- data_corrected[train_indices, ]
test_data <- data_corrected[-train_indices, ]


# Create a Random Forest model
model <- randomForest(fav_cuisine_coded ~ ., data = train_data, ntree = 1000)

# Make predictions on the test set
predictions <- predict(model, newdata = test_data)

# Evaluate the model's accuracy
accuracy <- sum(predictions == test_data$fav_cuisine_coded) / nrow(test_data)
cat("Random Forest Model Accuracy:", accuracy)

print(predictions)

# Combine predicted and actual values into a data frame
result <- data.frame(predictions, test_data$fav_cuisine_coded)


rownames(result) <- 1:nrow(result)

# Print the comparison of predicted and actual values
print(result)

# Create a plot to compare predicted and actual values
result$Index <- rownames(result)
result$Index <- as.numeric(result$Index)

ggplot(result, aes(x = Index)) +
  geom_point(aes(y = predictions), color = "blue", size = 3, shape = 3) +
  geom_point(aes(y = test_data$fav_cuisine_coded), color = "red", size = 3, shape = 4) +
  labs(x = "Index", y = "Value") +
  ggtitle("Comparison of Predictions and Real Data") +
  scale_x_continuous(breaks = result$Index)



