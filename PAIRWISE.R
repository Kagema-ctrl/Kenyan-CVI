# Define the criteria based on coastal vulnerability indicators
criteria <- c("Sea Level Rise", "Shoreline Change Rate", "Coastal Geomorphology", 
              "Mean Significant Wave Height", "Bathymetry", "Mean Tidal Range", "Coastal Slope", "Coastal Elevation")

# Create a valid reciprocal pairwise comparison matrix
comparison_matrix <- matrix(c(1,    1,    5,    7,    5,    7,    7,    9,
                              1,    1,    3,    5,    3,    5,    5,    7,
                              1/5,  1/3,  1,    3,    1,    3,    3,    5,
                              1/7,  1/5,  1/3,  1,    1,    3,    3,    3,
                              1/5,  1/3,  1,    1,    1,    1,    3,    3,
                              1/7,  1/5,  1/3,  1/3,  1,    1,    1,    3,
                              1/7,  1/5,  1/3,  1/3,  1/3,  1,    1,    3,
                              1/9,  1/7,  1/5,  1/3,  1/3,  1/3,  1/3,  1), 
                            nrow = 8, byrow = TRUE)

# Set row and column names to the matrix
rownames(comparison_matrix) <- criteria
colnames(comparison_matrix) <- criteria

# Ensure the matrix is symmetric (reciprocal property)
comparison_matrix <- (comparison_matrix + t(1 / comparison_matrix)) / 2

# Calculate weights using eigenvector method
eigen_values <- eigen(comparison_matrix)
principal_eigenvalue <- max(Re(eigen_values$values))  # Extract the real part of the maximum eigenvalue
weights <- Re(eigen_values$vectors[,1]) / sum(Re(eigen_values$vectors[,1]))  # Normalize eigenvector

# Print the weights
print("Weights for each criterion based on pairwise comparison:")
print(weights)

# Calculate the consistency ratio to check the consistency of the matrix
CI <- (principal_eigenvalue - nrow(comparison_matrix)) / (nrow(comparison_matrix) - 1)
RI_values <- c(0, 0, 0.58, 0.9, 1.12, 1.24, 1.32, 1.41)  # Random Consistency Index values for n=1-8
RI <- RI_values[length(criteria)]
CR <- CI / RI

# Print consistency ratio
print(paste("Consistency Ratio:", CR))

# Check if the comparison matrix is consistent
if(CR < 0.1) {
  print("The matrix is consistent.")
} else {
  print("The matrix is not consistent. Review comparisons.")
}

# Create a data frame for the weights with two columns: Indicator and Weight
weight_table <- data.frame(
  Indicator = criteria,
  Weight = round(weights, 4)  # Rounded to 4 decimal places for better readability
)


# Check if necessary packages are installed and install them if not
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(gridExtra)) install.packages("gridExtra")

# Load libraries
library(ggplot2)
library(gridExtra)

# Create a data frame for the weights with two columns: Indicator and Weight
weight_table <- data.frame(
  Indicator = criteria,
  Weight = round(weights, 4)  # Rounded to 4 decimal places for better readability
)

# Print the weight table
print("Table of Weights for Each Indicator:")
print(weight_table)

# Create a table plot
table_plot <- tableGrob(weight_table)

# Save the table as an image
output_image <- "Weight_Table.png"
png(output_image, width = 800, height = 600)  # Set dimensions for the image
grid.draw(table_plot)  # Draw the table
dev.off()  # Close the PNG device

# Notify the user about the file export
print(paste("The weight table has been exported as an image to", output_image))

getwd()
