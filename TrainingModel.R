# Load dataset
breast_cancer_data <- read.csv("data/Breast_cancer_data.csv", colClasses = c(
  mean_radius = "numeric",
  mean_texture = "numeric",
  mean_perimeter = "numeric",
  mean_area = "numeric",
  mean_smoothness = "numeric",
  diagnosis = "factor"
))

# Display the structure of the dataset
str(breast_cancer_data)

# View the first few rows of the dataset
head(breast_cancer_data)

# View the dataset in a separate viewer window
View(breast_cancer_data)

# Install and load the caTools package
install.packages("caTools")
library(caTools)

# Set seed for reproducibility
set.seed(123)

# Split the data into training (70%) and testing (30%) sets
split <- sample.split(breast_cancer_data$diagnosis, SplitRatio = 0.7)
training_set <- subset(breast_cancer_data, split == TRUE)
testing_set <- subset(breast_cancer_data, split == FALSE)

# Display the structure of the training and testing sets
str(training_set)
str(testing_set)
dim(training_set)
dim(testing_set)

# Install and load the boot package
install.packages("boot")
library(boot)

# Define a function to compute the statistic of interest (mean in this case)
bootstrap_function <- function(data, indices) {
  d <- data[indices, ]  # allows boot to select sample
  return(mean(d$mean_radius))  # example statistic
}

# Apply bootstrapping
set.seed(123)
bootstrap_results <- boot(data = breast_cancer_data, statistic = bootstrap_function, R = 1000)

# Display bootstrapping results
print(bootstrap_results)
plot(bootstrap_results)

# Install and load the caret package
install.packages("caret")
library(caret)

# Set up cross-validation
train_control <- trainControl(method = "cv", number = 10)

# Train a model using cross-validation (example with logistic regression)
set.seed(123)
model <- train(diagnosis ~ mean_radius + mean_texture + mean_perimeter + mean_area + mean_smoothness,
               data = training_set,
               method = "glm",
               family = binomial,
               trControl = train_control)

# Print the model summary
print(model)

# Make predictions on the testing set
predictions <- predict(model, newdata = testing_set)

# Evaluate the model's performance
confusion_matrix <- confusionMatrix(predictions, testing_set$diagnosis)
print(confusion_matrix)
