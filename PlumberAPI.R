# Load the saved SVM model
loaded_model_svm <- readRDS("./models/svm_model.rds")

#* @apiTitle Breast Cancer Diagnosis Prediction API
#* @apiDescription Used to predict breast cancer diagnosis.

#* @param mean_radius Mean radius of the tumor
#* @param mean_texture Mean texture of the tumor
#* @param mean_perimeter Mean perimeter of the tumor
#* @param mean_area Mean area of the tumor
#* @param mean_smoothness Mean smoothness of the tumor

#* @post /predict_breast_cancer_diagnosis

predict_breast_cancer_diagnosis <- function(mean_radius, mean_texture, mean_perimeter, mean_area, mean_smoothness) {
  # Create a data frame using the arguments
  new_data <- data.frame(
    mean_radius = as.numeric(mean_radius),
    mean_texture = as.numeric(mean_texture),
    mean_perimeter = as.numeric(mean_perimeter),
    mean_area = as.numeric(mean_area),
    mean_smoothness = as.numeric(mean_smoothness)
  )
  
  # Use the loaded model to make predictions for new data
  predictions <- predict(loaded_model_svm, newdata = new_data)
  
  # Return predictions
  return(predictions)
}
