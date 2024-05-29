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
