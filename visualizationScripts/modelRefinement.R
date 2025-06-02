################################################################################
# setup
################################################################################

# clear memory
rm(list = ls())

# packages
# ------------------------------------------------------------------------------

# install.packages("ggnewscale")
# install.packages("ggplot2")
# install.packages("car")
# install.packages("tidyverse")
# install.packages("ggcorrplot")
# install.packages("dplyr")
# install.packages("corrplot")
# install.packages("GGally")
# install.packages("lares")
# install.packages("pROC")
# install.packages("data.table")
# install.packages("ggdist")
library(car)
library(tidyverse)
library(ggcorrplot)
library(dplyr)
library(corrplot)
library(GGally)
library(lares)
library(pROC)
library(data.table)
library(ggplot2)
library(ggnewscale)
library(ggdist)

# working directory
# ------------------------------------------------------------------------------

# Get the directory of the currently running R script
script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)

# set as the working directory
setwd(script_dir)
# print(script_dir)

# change working directory to the folder with the testing point csv files 
# (exported from GEE)
setwd("../data/modelRefinement")
getwd()

# nice labels
# ------------------------------------------------------------------------------

# Create a vector matching nice labels to current column names
label_clean <- c(
  full = "full model",
  EVI = "EVI",
  HHpalsarLdiffWetDry = "Seasonal Diff in SAR ",
  NDMI = "NDMI",
  NDWI = "NDWI",
  PDSI = "PDSI",
  VPD = "VPD",
  aspect = "Aspect",
  canopyCover = "Canopy Cover",
  canopyCoverChange = "Canopy Cover Change",
  canopyHeightChange = "Canopy Height Change",
  distanceBambooForest = "Dist to Bamboo Forest",
  distanceBurn1yrBefore = "Dist to Burn (1 Year Prior)",
  distanceBurn2yrBefore = "Dist to Burn (2 Years Prior)",
  distanceDryDipterocarpForest = "Dist to Dry Dipterocarp Forest",
  distanceDryEvergreenForest = "Dist to Dry Evergreen Forest",
  distanceEucalyptusPlantation = "Dist to Eucalyptus Plantation",
  distanceHillEvergreenForest = "Dist to Hill Evergreen Forest",
  distanceMaize = "Dist to Maize ",
  distanceMixedDeciduousForest = "Dist to Mixed Deciduous Forest",
  distanceOldClearing = "Dist to Old Clearing",
  distancePineForest = "Dist to Pine Forest",
  distanceProtectedArea = "Dist to DNP & RFD",
  distanceRice = "Dist to Rice ",
  distanceRoads = "Dist to Roads",
  distanceSPKKTC = "Dist to SPK & KTC",
  distanceSecondaryGrowthForest = "Dist to Secondary Growth Forest",
  distanceSettlements = "Dist to Settlements",
  distanceSugarcane = "Dist to Sugarcane",
  distanceTeakPlantation = "Dist to Teak Plantation",
  distanceWater = "Dist to Water",
  elevation = "Elevation",
  flameLength = "Flame Length",
  fuelLoad = "Fuel Load",
  grassHeight = "Grass Height",
  litterCover = "Litter Cover",
  litterDepth = "Litter Depth",
  populationCount = "Population Count",
  precipitation = "Precipitation",
  rateOfSpread = "Rate of Fire Spread",
  slope = "Slope",
  soilMoisture = "Soil Moisture",
  temperatureMax = "Max Temperature"
)

# print(label_clean)

################################################################################
# plot AUCs (model refinement)
################################################################################

auc = read.csv("AUC_v17_modelRefinement.csv")

# clean the dataframe
auc_clean <- auc %>%
  # arrange from high to low values
  arrange(desc(AUC)) %>%
  # Rename the items in the variable column using the "label_clean" table
  mutate(Removed = label_clean[as.character(Removed)]) %>%
  # Convert 'Removed' to a factor and preserve the current order in plot
  mutate(Removed = factor(Removed, levels = Removed))
# print(auc_clean)

# Create line plot of AUC by removed variable
ggplot(auc_clean, 
       aes(x = Removed,
           y = AUC, 
           group = 1)) +
  geom_vline(xintercept = 12.5, 
             linetype = "dashed", 
             color = "#e1a486",
             size = 1) +
  annotate("rect",
           xmin = 12.5, 
           xmax = Inf,
           ymin = -Inf, 
           ymax = Inf,
           fill = "#e1a486", 
           alpha = 0.2) +
  geom_line(color = "#9c3400ff", 
            size = 1) +
  geom_point(size = 2.5, 
             color = "#9c3400ff") +
  annotate("text", 
           x = 20, 
           y = 0.69, 
           label = "Variables retained in final model", 
           fontface = "bold",
           color = "#e1a486", 
           hjust = 0) +
  # Add arrow pointing to x = 5, y = 25
  annotate("segment",
           x = 9.5, 
           y = 0.78,        
           xend = 12.2, 
           yend = 0.831,  
           colour = "#e1a486",
           arrow = arrow(length = unit(0.15, 
                                       "inches")),
           size = 1) +
  annotate("text",
           x = 2, y = 0.765,
           label = "Distinct drop in AUC",
           fontface = "bold",
           hjust = 0, color = "#e1a486") +
  theme_minimal() +
  labs(title = "AUC by Removed Variable",
       x = "Removed Variable", 
       y = "AUC",
       subtitle = "From Model Refinement") +
  theme(axis.text.x = element_text(angle = 70, 
                                   hjust = 1,
                                   size = 8),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
