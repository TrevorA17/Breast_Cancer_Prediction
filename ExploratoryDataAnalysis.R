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

# Frequency distribution for the diagnosis
diagnosis_freq <- table(breast_cancer_data$diagnosis)
print(diagnosis_freq)

# Proportion of each class
diagnosis_prop <- prop.table(diagnosis_freq)
print(diagnosis_prop)

# Mean for numeric variables
mean_values <- sapply(breast_cancer_data[, 1:5], mean)
print(mean_values)

# Median for numeric variables
median_values <- sapply(breast_cancer_data[, 1:5], median)
print(median_values)

# Mode function
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Since mode is not typically used for numeric variables in this context, we'll skip it for now.
# Variance for numeric variables
variance_values <- sapply(breast_cancer_data[, 1:5], var)
print(variance_values)

# Standard deviation for numeric variables
sd_values <- sapply(breast_cancer_data[, 1:5], sd)
print(sd_values)

# Skewness and kurtosis require the 'moments' package
install.packages("moments")
library(moments)

# Skewness for numeric variables
skewness_values <- sapply(breast_cancer_data[, 1:5], skewness)
print(skewness_values)

# Kurtosis for numeric variables
kurtosis_values <- sapply(breast_cancer_data[, 1:5], kurtosis)
print(kurtosis_values)

# Correlation matrix for numeric variables
correlation_matrix <- cor(breast_cancer_data[, 1:5])
print(correlation_matrix)

# Scatter plot matrix
pairs(breast_cancer_data[, 1:5], main = "Scatterplot Matrix")

# ANOVA for mean_radius
anova_mean_radius <- summary(aov(mean_radius ~ diagnosis, data = breast_cancer_data))
print(anova_mean_radius)

# ANOVA for mean_texture
anova_mean_texture <- summary(aov(mean_texture ~ diagnosis, data = breast_cancer_data))
print(anova_mean_texture)

# ANOVA for mean_perimeter
anova_mean_perimeter <- summary(aov(mean_perimeter ~ diagnosis, data = breast_cancer_data))
print(anova_mean_perimeter)

# ANOVA for mean_area
anova_mean_area <- summary(aov(mean_area ~ diagnosis, data = breast_cancer_data))
print(anova_mean_area)

# ANOVA for mean_smoothness
anova_mean_smoothness <- summary(aov(mean_smoothness ~ diagnosis, data = breast_cancer_data))
print(anova_mean_smoothness)
