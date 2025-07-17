# Load necessary libraries
if (!require("sf")) install.packages("sf")
if (!require("dplyr")) install.packages("dplyr")

library(sf)
library(dplyr)

# Step 1: Load the shapefile
shapefile_path <- "C:/Users/USER/Desktop/PROJECT/DATA/CVI/CVI.shp"
cvi_data <- st_read(shapefile_path)

# Step 2: Verify field names
colnames(cvi_data)  # Check column names

# Step 3: Check and convert field types to numeric
cvi_data <- cvi_data %>%
  mutate(
    PVI_nrm = as.numeric(PVI_nrm),
    SVI_nrm = as.numeric(SVI_nrm)
  )

# Step 4: Handle missing values
cvi_data <- cvi_data %>%
  mutate(
    PVI_nrm = ifelse(is.na(PVI_nrm), 0, PVI_nrm),
    SVI_nrm = ifelse(is.na(SVI_nrm), 0, SVI_nrm)
  )

# Step 5: Define weights for PVI and SoVI
Wp <- 0.7  # Weight for PVI
Ws <- 0.3  # Weight for SoVI

# Step 6: Calculate CVI
cvi_data <- cvi_data %>%
  mutate(
    CVI = (PVI_nrm * Wp) + (SVI_nrm * Ws)  # CVI formula
  )

# Step 7: Save the updated shapefile
output_shapefile <- "C:/Users/USER/Desktop/PROJECT/DATA/CVI/Final_CVI.shp"
st_write(cvi_data, output_shapefile, delete_layer = TRUE)

# Notify the user
cat("CVI has been calculated and added to the shapefile. The updated shapefile has been saved at:", output_shapefile, "\n")
