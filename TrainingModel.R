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
