################################################################################
# setup
################################################################################

# clear memory
rm(list = ls())

# packages
# ------------------------------------------------------------------------------

# Load necessary libraries
# install.packages("dplyr")
# install.packages("tidyverse")
# install.packages("readr")
# install.packages("ggplot2")
library(ggplot2)
library(readr)
library(tidyverse)
library(dplyr)

# working directory
# ------------------------------------------------------------------------------

# Get the directory of the currently running R script
script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)

# set as the working directory
setwd(script_dir)
# print(script_dir)

# change working directory to the folder with the testing point csv files 
# (exported from GEE)
setwd("../data/variableImportance")
getwd()

# nice labels
# ------------------------------------------------------------------------------

# Create a named vector of nice labels
label_clean <- c(
  EVI = "EVI (Enhanced Vegetation Index)",
  HHpalsarLdiffWetDry = "Seasonal Difference in HH SAR Signal",
  NDMI = "NDMI (Normalized Difference Moisture Index)",
  NDWI = "NDWI (Normalized Difference Water Index)",
  PDSI = "PDSI (Palmer Drought Severity Index)",
  VPD = "VPD (Vapor Pressure Deficit)",
  aspect = "Aspect",
  canopyCover = "Canopy Cover",
  canopyCoverChange = "Canopy Cover Change",
  canopyHeightChange = "Canopy Height Change",
  distanceBambooForest = "Distance to Bamboo Forest",
  distanceBurn1yrBefore = "Distance to Burn (1 Year Prior)",
  distanceBurn2yrBefore = "Distance to Burn (2 Years Prior)",
  distanceDryDipterocarpForest = "Distance to Dry Dipterocarp Forest",
  distanceDryEvergreenForest = "Distance to Dry Evergreen Forest",
  distanceEucalyptusPlantation = "Distance to Eucalyptus Plantation",
  distanceHillEvergreenForest = "Distance to Hill Evergreen Forest",
  distanceMaize = "Distance to Maize ",
  distanceMixedDeciduousForest = "Distance to Mixed Deciduous Forest",
  distanceOldClearing = "Distance to Old Clearing",
  distancePineForest = "Distance to Pine Forest",
  distanceProtectedArea = "Distance to DNP & RFD Areas",
  distanceRice = "Distance to Rice ",
  distanceRoads = "Distance to Roads",
  distanceSPKKTC = "Distance to SPK & KTC Areas",
  distanceSecondaryGrowthForest = "Distance to Secondary Growth Forest",
  distanceSettlements = "Distance to Settlements",
  distanceSugarcane = "Distance to Sugarcane",
  distanceTeakPlantation = "Distance to Teak Plantation",
  distanceWater = "Distance to Water",
  elevation = "Elevation",
  flameLength = "Flame Length",
  fuelLoad = "Fuel Load",
  grassHeight = "Grass Height",
  litterCover = "Litter Cover",
  litterDepth = "Litter Depth",
  populationCount = "Population Density",
  precipitation = "Precipitation",
  rateOfSpread = "Rate of Fire Spread",
  slope = "Slope",
  soilMoisture = "Soil Moisture",
  temperatureMax = "Maximum Temperature"
)

################################################################################
# plot variable variable importances (full model)
################################################################################

# import full model and refined model variable importances
varImp_v17 = read.csv("variableImportance_v17.csv")
print(names(varImp_v17))

# clean up
varImp_v17_clean <- varImp_v17 %>% 
  # remove undesired columns
  select(-.geo,-system.index) %>%
  # flip columns and rows, rename new columns
  pivot_longer(cols = everything(), 
               names_to = "Variable", 
               values_to = "Importance") %>% 
  # arrange by high to low importance
  arrange(desc(Importance)) %>%
  # replace variable names with the clean versions
  mutate(Variable = label_clean[as.character(Variable)]) %>%
  # turn variable into a factor to maintain order in plot
  mutate(Variable = factor(Variable,
                           levels = rev(Variable)))

# Create bar chart with ggplot
plot = ggplot(varImp_v17_clean,             
       aes(x = Variable,             # Set x-axis to variables
           y = Importance)) +        # Set y-axis to importances
  geom_bar(stat = "identity",        # add bar chart
           fill = "#9c3400ff") +     # set bar color
  coord_flip() +                     # Flip coordinates (horizontal bars)
  theme_minimal() +                  # minimal theme
  geom_text(aes(label = round(Importance, 2)), # add text labels
            hjust = -0.1,            # Position labels slightly outside bars
            size = 3) +              # Set text size
  labs(
    title = "Scaled Variable Importance",       # Title
    subtitle = "Before Variable Removal",       # Subtitle
    x = "Variable",                             # X-axis label
    y = "Importance"                            # Y-axis label
  ) +
  scale_y_continuous(expand = c(0, 0),          # No space below bars
                     limits = c(0, max(varImp_v17_clean$Importance) + 0.3)) + # Set y-axis limit
  theme(
    text = element_text(size = 8),                       # Text size
    plot.title = element_text(hjust = 0.5, size = 16),   # title
    plot.subtitle = element_text(hjust = 0.5,size = 14), #subtitle
    axis.title.x = element_text(size = 12),              # X-axis label 
    axis.title.y = element_text(size = 12),              # Y-axis label 
    axis.text.x = element_text(size = 8),                # X-axis tick text 
    axis.text.y = element_text(size = 8)                 # Y-axis tick text
  )

print(plot)

ggsave(filename = "varImportance_full.png",        
       plot = plot,     
       bg = "white",
       width = 9, height = 6, dpi = 1000)

################################################################################
# plot variable variable importances (refined model)
################################################################################

# import
varImp_v17_11 = read.csv("variableImportance_v17_11.csv")
print(names(varImp_v17_11))

# clean up
varImp_v17_11_clean <- varImp_v17_11 %>% 
  # remove undesired columns
  select(-.geo,-system.index) %>%
  # flip columns and rows, rename new columns
  pivot_longer(cols = everything(), 
               names_to = "Variable", 
               values_to = "Importance") %>% 
  # arrange by high to low importance
  arrange(desc(Importance)) %>%
  # replace variable names with the clean versions
  mutate(Variable = label_clean[as.character(Variable)]) %>%
  # turn variable into a factor to maintain order in plot
  mutate(Variable = factor(Variable,
                           levels = rev(Variable)))

# Create bar chart
plot = ggplot(varImp_v17_11_clean, 
       aes(x = Variable, 
           y = Importance)) +
  geom_bar(stat = "identity", 
           fill = "#9c3400ff") +
  coord_flip() +  
  geom_text(aes(label = round(Importance, 2)), 
            hjust = -0.1, 
            size = 3) +
  theme_minimal() +
  labs(
    title = "Scaled Variable Importance",
    subtitle = "After Variable Removal",
    x = "Variable",
    y = "Importance"
  ) +
  scale_y_continuous(expand = c(0, 0), 
                     limits = c(0, max(varImp_v17_11_clean$Importance) + 0.3)) + 
  theme(
    text = element_text(size = 8),                      
    plot.title = element_text(hjust = 0.5, size = 16),   
    plot.subtitle = element_text(hjust = 0.5,size = 14), 
    axis.title.x = element_text(size = 12),               
    axis.title.y = element_text(size = 12),              
    axis.text.x = element_text(size = 8),               
    axis.text.y = element_text(size = 8)                
  )

print(plot)

ggsave(filename = "varImportance_refined.png",        
       plot = plot,     
       bg = "white",
       width = 9, height = 6, dpi = 1000)
