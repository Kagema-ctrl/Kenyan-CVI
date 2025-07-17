# Load necessary libraries
if (!require("sf")) install.packages("sf")
if (!require("dplyr")) install.packages("dplyr")

library(sf)
library(dplyr)

# Step 1: Load the shapefile
shapefile_path <- "C:/Users/USER/Desktop/PROJECT/DATA/CVI/RANKEDSOCIALINDICATORS.shp"
coastline <- st_read(shapefile_path)

# Step 2: Define weights for each indicator
weights <- list(
  LULC = 0.637,
  Population = 0.2583,
  Distance_of_Roads = 0.1047
)

# Step 3: Add columns for max and min values for normalization
coastline <- coastline %>%
  mutate(
    Max_LULC = max(LULC_Class, na.rm = TRUE),
    Min_LULC = min(LULC_Class, na.rm = TRUE),
    Max_POP = max(gridcode, na.rm = TRUE),
    Min_POP = min(gridcode, na.rm = TRUE),
    Max_ROADS = max(NEAR_DIST, na.rm = TRUE),
    Min_ROADS = min(NEAR_DIST, na.rm = TRUE)
  )

# Step 4: Calculate SoVI for each segment
coastline <- coastline %>%
  mutate(
    SoVI = (
      ((RANK_LULC - Min_LULC) / (Max_LULC - Min_LULC)) * weights$LULC +
        ((RANK_POP - Min_POP) / (Max_POP - Min_POP)) * weights$Population +
        ((RANK_ROADS - Min_ROADS) / (Max_ROADS - Min_ROADS)) * weights$Distance_of_Roads
    )
  )

# Step 5: Shift the SoVI range to start from 0
SoVI_min <- min(coastline$SoVI, na.rm = TRUE)
coastline <- coastline %>%
  mutate(
    SoVI_shifted = SoVI - SoVI_min  # Shift so the minimum value is 0
  )

# Step 6: Normalize the shifted SoVI to a scale of 1–100
SoVI_max <- max(coastline$SoVI_shifted, na.rm = TRUE)
coastline <- coastline %>%
  mutate(
    SoVI_normalized = ((SoVI_shifted) / SoVI_max) * 99 + 1  # Normalize to 1–100
  )

# Step 7: Save the updated shapefile
output_shapefile <- "C:/Users/USER/Desktop/PROJECT/DATA/CVI/Updated_SoVI.shp"
st_write(coastline, output_shapefile, delete_layer = TRUE)

# Notify the user
cat("SoVI has been calculated, shifted, normalized, and added to the shapefile. The updated shapefile has been saved at:", output_shapefile, "\n")
