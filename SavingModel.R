# Load necessary libraries
library(caret)
library(e1071)  # For SVM

# Define control for cross-validation
control <- trainControl(method = "cv", number = 10)

# Train SVM model
set.seed(123)  # For reproducibility
model_svm <- train(diagnosis ~ ., data = breast_cancer_data, method = "svmRadial", trControl = control)

# Create a directory named "models" if it doesn't exist
if (!file.exists("./models")) {
  dir.create("./models")
}

# Save the trained SVM model to a file
saveRDS(svm_model, file = "./models/svm_model.rds")

# Load the saved SVM model
loaded_model_svm <- readRDS("./models/svm_model.rds")

# Prepare new data for prediction
new_data <- data.frame(
  mean_radius = 17.99,
  mean_texture = 10.38,
  mean_perimeter = 122.8,
  mean_area = 1001,
  mean_smoothness = 0.1184
)

# Use the loaded model to make predictions for new data
predictions_loaded_model <- predict(loaded_model_svm, newdata = new_data)

# Print predictions
print(predictions_loaded_model)
