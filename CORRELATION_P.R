install.packages("ggplot2")
install.packages("Hmisc")
install.packages("reshape2")
install.packages("readr")

library(readr)

# Import the CSV file
data <- read_csv("C:/Users/USER/Desktop/PROJECT/DATA/CVI/INDICATORS.csv", show_col_types = FALSE)

# Check the first few rows of the dataframe
head(data)

# Check for missing values
sum(is.na(data))

# to remove rows with any missing values:
data <- na.omit(data)

# Load necessary library for statistical operations
library(Hmisc)

# Pearson correlation for normally distributed data
cor_matrix <- cor(data, method = "pearson")


# View the correlation matrix
print(cor_matrix)

# Load necessary library for visualization
library(ggplot2)
library(reshape2)

# Melt the correlation matrix for ggplot
melted_cor_matrix <- melt(cor_matrix)

# Create a heatmap
ggplot(melted_cor_matrix, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(midpoint = 0, low = "blue", mid = "white", high = "red", limits = c(-1, 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = '', y = '', fill = 'Correlation')
