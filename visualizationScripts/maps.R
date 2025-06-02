################################################################################
# setup
################################################################################

# clear memory
rm(list = ls())

# packages
# ------------------------------------------------------------------------------

# Load packages
# install.packages('tmap')
# install.packages('terra')
library(terra)
library(tmap)

# Set to plot mode
tmap_mode("plot")  # or "view" for interactive

# working directory
# ------------------------------------------------------------------------------

# Get the directory of the currently running R script
script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)

# set as the working directory
setwd(script_dir)
# print(script_dir)

# change working directory to the folder with the testing point csv files 
# (exported from GEE)
setwd("../data/maps")
getwd()

################################################################################
# load and clean data
################################################################################

# load predictor raster
r <- rast("predictors_staticTemporalRawVal_2024_v10.tif")  
# print(names(r))

# remove bands not used as predictors
r <- r[[!names(r) %in% c(
  'canopyHeight',
  'temperatureMin',
  'temperatureMean',
  'VVsentinel1Cdry',
  'HHpalsarLdry',
  'VVsentinel1Cwet',
  'HHpalsarLwet',
  'VVsentinel1CdiffWetDry',
  'blue',
  'green',
  'red',
  'nir',
  'temp',
  'swir1',
  'swir2',
  'NDVI',
  'distanceInundatedForest',
  'distanceMoistEvergreenForest',
  'distanceBeachForest',
  'distanceMangroveForest',
  'distanceSwampForest')]]
print(names(r))

# distance bands
# ------------------------------------------------------------------------------

# define distance bands
distance_bands <- c(
  "distanceBurn1yrBefore", "distanceBurn2yrBefore", "distanceDryDipterocarpForest",
  "distanceDryEvergreenForest", "distanceEucalyptusPlantation", "distanceHillEvergreenForest",
  "distanceMaize", "distanceMixedDeciduousForest", "distanceOldClearing", "distancePineForest",
  "distanceProtectedArea", "distanceRice", "distanceRoads", "distanceSPKKTC", "distanceSettlements",
  "distanceSugarcane", "distanceTeakPlantation", "distanceWater", "distanceBambooForest",
  "distanceSecondaryGrowthForest"
)

# Convert distance from meters to kilometers
r[[distance_bands]] <- r[[distance_bands]] / 1000

# climate bands
# ------------------------------------------------------------------------------

# Convert max temp to C from scaled value
r[['temperatureMax']] <- r[['temperatureMax']] * 0.1  

# Convert PDSI to true number from scaled value
r[['PDSI']] <- r[['PDSI']] * 0.01  

# Convert VPD to kPa from scaled value
r[['VPD']] <- r[['VPD']] * 0.01 

# Convert soil moisture to mm from scaled value
r[['soilMoisture']] <- r[['soilMoisture']] * 0.1  

# fuel bands
# ------------------------------------------------------------------------------

# Convert Fuel Load from tons/acre → tons/hectare
# 1 ton/acre = 2.47105 tons/hectare
r[["fuelLoad"]] <- r[["fuelLoad"]] * 2.47105  

# Convert Grass Height from inches → centimeters
# 1 inch = 2.54 cm
r[["grassHeight"]] <- r[["grassHeight"]] * 2.54 

# Convert Litter Depth from feet → centimeters
# 1 foot = 30.48 cm
r[["litterDepth"]] <- r[["litterDepth"]] * 30.48  

################################################################################
# create maps
################################################################################

# Define band names
band_names <- names(r)
# print(band_names)

# create nice legend titles
legend_titles <- c(
  "Canopy Cover (%)",
  "Dist to Burn (1 Yr Prior) (km)",
  "Dist to Burn (2 Yrs Prior) (km)",
  "Precipitation (mm)",
  "Max Temperature (°C)",
  "Soil Moisture (mm)",
  "PDSI",
  "VPD (kPa)",
  "Canopy Cover Change (%)",
  "Canopy Height Change (m)",
  "Seasonal Diff in SAR (dB)",
  "EVI",
  "NDMI",
  "NDWI",
  "Population Count (people/ha)",
  "Dist to Dry Evergreen Forest (km)",
  "Dist to Hill Evergreen Forest (km)",
  "Dist to Pine Forest (km)",
  "Dist to Mixed Decid Forest (km)",
  "Dist to Dry Dipterocarp Forest (km)",
  "Dist to Bamboo Forest (km)",
  "Dist to Teak Plantation (km)",
  "Dist to Secondary Growth Forest (km)",
  "Dist to Old Clearing (km)",
  "Dist to Eucalyptus Plantation (km)",
  "Dist to Maize (km)",
  "Dist to Sugarcane (km)",
  "Dist to Rice (km)",
  "Dist to Water (km)",
  "Dist to DNP & RFD (km)",
  "Dist to SPK & KTC (km)",
  "Dist to Roads (km)",
  "Dist to Settlements (km)",
  "Slope (degr)",
  "Elevation (m)",
  "Aspect (degr)",
  "Fuel Load (tons/ha)",
  "Grass Height (cm)",
  "Litter Cover (%)",
  "Litter Depth (cm)",
  "Flame Length (m)",
  "Rate of Spread (m/min)"
)
# print(legend_titles)

# create a list of palettes for visualization
palette_list <- list(
  # Canopy Cover (%)
  c("white","#006e52"),
  # Distance to Burn (1 Yr Prior) (km)
  c("#b33c00", "white","white"),
  # Distance to Burn (2 Yr Prior) (km)
  c("#b33c00", "white","white"),
  # Precipitation (mm)
  c("white","#9c0049"),
  # Max Temperature (°C)
  c("white","white","#9c0049"),
  # Soil Moisture (mm)
  c("white","#9c0049"),
  # PDSI
  c("#9c0049","white"),
  # VPD (kPa)
  c("white","#9c0049"),
  # Canopy Cover Change (m)
  c("white","#006e52"),
  # Canopy Height Change (m)
  c("white","#006e52"),
  # Seasonal Diff in SAR (dB)
  c("white","#006e52"),
  # EVI
  c("white","#006e52"),
  # NDMI
  c("white","white","#006e52"),
  # NDWI
  c("white","#007093","#007093"),
  # Population Count
  c("#2c0095", "white","white","white","white"),
  # Distance to forests (km)
  c("#3e8b00", "white"),
  c("#3e8b00", "white"),
  c("#3e8b00", "white"),
  c("#3e8b00", "white", "white"),
  c("#3e8b00", "white"),
  c("#3e8b00", "white"),
  c("#3e8b00", "white"),
  c("#3e8b00", "white"),
  c("#3e8b00", "white"),
  c("#3e8b00", "white"),
  # Distance to Maize (km)
  c("#939900", "white", "white"),
  # Distance to sugarcane (km)
  c("#939900", "white"),
  # Distance to rice (km)
  c("#939900", "white", "white"),
  # Distance to water (km)
  c("#007093", "white", "white"),
  # Dist to DNP & RFD (km),
  c("#2c0095", "white", "white"),
  # Dist to SPK & KTC (km),
  c("#2c0095", "white"),
  # Dist to Roads (km),
  c("#2c0095", "white", "white"),
  # Dist to Settlements (km),
  c("#2c0095", "white", "white"),
  # Slope (degrees)
  c("white", "black"),
  # Elevation (m)
  c("white", "black"),
  # Aspect (degrees)
  c("white", "black"),
  # Fuel Load (tons/ha)
  c("white", "#a87b00"),
  # Grass Height (cm)
  c("white", "#a87b00"),
  # Litter Cover (%)
  c("white", "#a87b00"),
  # Litter Depth (cm)
  c("white", "#a87b00"),
  # Flame Length (m)
  c("white", "#a00000"),
  # Rate of Fire Spread (m/min)
  c("white", "#a00000")
)
# print(palette_list)

# Loop through bands and export maps
for (i in 1:nlyr(r)) {
  
  # extract the band of interest
  band <- r[[i]]
  # get the band name
  band_name <- band_names[i]
  # get the clean legend title for that band
  title <- legend_titles[i]
  # get the color palette for that band
  palette <- palette_list[[i]]
  
  # create a map object
  map <- tm_shape(band) +
    tm_raster( # add band as raster
      col.scale = tm_scale_continuous(values = palette), # color palette
      col.legend = tm_legend(title = title, # legend title
                             ticks = FALSE) # no ticks
    ) +
    tm_grid( # add lat/lon grid
      lines = FALSE, # no lines
      x = seq(floor(ext(band)[1]), # x breaks
              ceiling(ext(band)[2]),
              by = 1), 
      y = seq(floor(ext(band)[3]), # y breaks
              ceiling(ext(band)[4]), 
              by = 1),
      labels.inside.frame = FALSE, # labels outside frame
      label.format = list(digits = 0, suffix = "°"), # round to nearest integer
      col = "black"
    ) +
    tm_layout( # set layout
      legend.outside = TRUE, # legend 
      legend.frame = FALSE,                   
      legend.position = c(0.59, 0.46),
      legend.height = 15,
      legend.text.size = 0.6,            
      legend.title.size = 0.6 ,
      frame = TRUE #frame
    ) +
    tm_compass(position = c(0.87, 0.20)) + # add compass
    tm_scalebar(position = c(0.61, 0.08), # add scalebar
                text.size = 0.6)
  
  print(map)
  
  # # create filename
  # filename <- paste0("band_", i, "_", gsub("[^[:alnum:]_]", "_", band_name), ".jpg")
  # # export
  # tmap_save(map, 
  #           filename = filename, 
  #           width = 5, 
  #           height = 6, 
  #           units = "in", 
  #           dpi = 1000)
}










