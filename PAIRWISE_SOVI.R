# Define the criteria based on social vulnerability indicators
criteria <- c("LULC", "Population", "Distance of Road Networks from Shoreline")

# Create the reciprocal pairwise comparison matrix
comparison_matrix <- matrix(c(
  1,    3,    5,    # LULC row
  1/3,  1,    3,    # Population row
  1/5,  1/3,  1     # Distance of Road Networks row
), nrow = 3, byrow = TRUE)

# Set row and column names to the matrix
rownames(comparison_matrix) <- criteria
colnames(comparison_matrix) <- criteria

# Calculate weights using the eigenvector method
eigen_values <- eigen(comparison_matrix)
principal_eigenvalue <- max(Re(eigen_values$values))  # Extract the real part of the maximum eigenvalue
weights <- Re(eigen_values$vectors[, 1]) / sum(Re(eigen_values$vectors[, 1]))  # Normalize the eigenvector

# Print the weights
cat("Weights for each criterion based on pairwise comparison:\n")
print(weights)

# Calculate the consistency ratio (CR) to check consistency of the matrix
CI <- (principal_eigenvalue - nrow(comparison_matrix)) / (nrow(comparison_matrix) - 1)
RI_values <- c(0, 0, 0.58, 0.9, 1.12, 1.24, 1.32, 1.41)  # Random Consistency Index (RI) for n=1 to 8
RI <- RI_values[nrow(comparison_matrix)]
CR <- CI / RI

# Print consistency ratio
cat("\nConsistency Ratio (CR):", CR, "\n")

# Check if the comparison matrix is consistent
if (CR < 0.1) {
  cat("The matrix is consistent.\n")
} else {
  cat("The matrix is not consistent. Please review the pairwise comparisons.\n")
}

# Create a data frame for the weights with two columns: Indicator and Weight
weight_table <- data.frame(
  Indicator = criteria,
  Weight = round(weights, 4)  # Rounded to 4 decimal places for better readability
)


# Print the weight table
cat("\nTable of Weights for Each Indicator:\n")
print(weight_table)

