# Load necessary libraries
if (!require("sf")) install.packages("sf")
if (!require("dplyr")) install.packages("dplyr")

library(sf)
library(dplyr)

# Step 1: Load the shapefile
shapefile_path <- "C:/Users/USER/Desktop/PROJECT/DATA/CVI/RANKED_INDICATORS.shp"
coastline <- st_read(shapefile_path)

# Step 2: Define weights for each indicator
weights <- list(
  Sea_Level_Rise = 0.3524,
  Shoreline_Change_Rate = 0.2608,
  Coastal_Geomorphology = 0.1154,
  Mean_Sig_Wave_Height = 0.0760,
  Bathymetry = 0.0801,
  Mean_Tidal_Range = 0.0486,
  Coastal_Slope = 0.0422,
  Coastal_Elevation = 0.0244
)

# Step 3: Add columns for max and min values for normalization
coastline <- coastline %>%
  mutate(
    Max_SLR = max(grid_cod_1, na.rm = TRUE),
    Min_SLR = min(grid_cod_1, na.rm = TRUE),
    Max_SLC = max(WLR, na.rm = TRUE),
    Min_SLC = min(WLR, na.rm = TRUE),
    Max_Geomo = max(GEOMORPH_V, na.rm = TRUE),
    Min_Geomo = min(GEOMORPH_V, na.rm = TRUE),
    Max_SIGW = max(MEAN_SIG_W, na.rm = TRUE),
    Min_SIGW = min(MEAN_SIG_W, na.rm = TRUE),
    Max_Bathy = max(grid_code, na.rm = TRUE),
    Min_Bathy = min(grid_code, na.rm = TRUE),
    Max_Tidal = max(TIDAL_RANG, na.rm = TRUE),
    Min_Tidal = min(TIDAL_RANG, na.rm = TRUE),
    Max_Slope = max(grid_code1, na.rm = TRUE),
    Min_Slope = min(grid_code1, na.rm = TRUE),
    Max_Eleva = max(grid_code_, na.rm = TRUE),
    Min_Eleva = min(grid_code_, na.rm = TRUE)
  )

# Step 4: Calculate PVI for each segment
coastline <- coastline %>%
  mutate(
    PVI = (
      ((RANK_SLR - Min_SLR) / (Max_SLR - Min_SLR)) * weights$Sea_Level_Rise +
        ((RANK_SLC - Min_SLC) / (Max_SLC - Min_SLC)) * weights$Shoreline_Change_Rate +
        ((RANK_GEOMO - Min_Geomo) / (Max_Geomo - Min_Geomo)) * weights$Coastal_Geomorphology +
        ((RANK_SIGWA - Min_SIGW) / (Max_SIGW - Min_SIGW)) * weights$Mean_Sig_Wave_Height +
        ((RANK_BATHY - Min_Bathy) / (Max_Bathy - Min_Bathy)) * weights$Bathymetry +
        ((RANK_TIDAL - Min_Tidal) / (Max_Tidal - Min_Tidal)) * weights$Mean_Tidal_Range +
        ((RANK_SLOPE - Min_Slope) / (Max_Slope - Min_Slope)) * weights$Coastal_Slope +
        ((RANK_ELEVA - Min_Eleva) / (Max_Eleva - Min_Eleva)) * weights$Coastal_Elevation
    )
  )

# Step 5: Shift the PVI range to start from 0
PVI_min <- min(coastline$PVI, na.rm = TRUE)
coastline <- coastline %>%
  mutate(
    PVI_shifted = PVI - PVI_min  # Shift so the minimum value is 0
  )

# Step 6: Normalize the shifted PVI to a scale of 1–100
PVI_max <- max(coastline$PVI_shifted, na.rm = TRUE)
coastline <- coastline %>%
  mutate(
    PVI_normalized = ((PVI_shifted) / PVI_max) * 99 + 1  # Normalize to 1–100
  )

# Step 7: Save the updated shapefile
output_shapefile <- "C:/Users/USER/Desktop/PROJECT/DATA/CVI/Updated_Coastline.shp"
st_write(coastline, output_shapefile, delete_layer = TRUE)

# Notify the user
cat("PVI has been calculated, shifted, normalized, and added to the shapefile. The updated shapefile has been saved at:", output_shapefile, "\n")
