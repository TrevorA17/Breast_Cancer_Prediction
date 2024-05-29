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

# Check for missing values in the entire dataset
total_missing_values <- sum(is.na(breast_cancer_data))
print(paste("Total missing values:", total_missing_values))

# Install and load the VIM package
install.packages("VIM")
library(VIM)

# Plot missing data
aggr(breast_cancer_data, col = c('navyblue', 'red'), numbers = TRUE, sortVars = TRUE, labels = names(breast_cancer_data), cex.axis = .7, gap = 3, ylab = c("Missing data", "Pattern"))
