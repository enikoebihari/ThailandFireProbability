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
setwd("../data/sensitivityAnalysis")
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
# plot variable importances (sensitivity analysis)
################################################################################

# read in variable importances from sensitivity analysis
varImp = read.csv("varImportances_v17_sensitivityAnalysis.csv")
# print(colnames(varImp))

# Clean up dataframe 
varImp_clean <- varImp %>%
  # Remove unnecessary columns
  select(-system.index, -.geo) %>% 
  # Reshape the dataframe:
  pivot_longer(
    cols = -Removed,          # keep "Removed" as a column
    names_to = "variable",    # create a new column called "Variable", move current column names there
    values_to = "importance"  # create a new column called "Importance", move current table values there
  ) %>%
  # Remove rows where 'importance' is NA
  filter(!is.na(importance))  %>%         
  # Rename the items in the variable column using the "label_clean" table
  mutate(variable = label_clean[as.character(variable)]) %>%
  # Group by variable to prepare for computing the median importance
  group_by(variable) %>%
  # Calculate the median importance for each variable
  mutate(median_importance = median(importance, na.rm = TRUE)) %>%
  # Ungroup
  ungroup() %>%
  # Reorder with high to low median importance
  mutate(variable = reorder(variable, -median_importance))  # descending order

# Calculate summary stats
varImp_summary <- varImp_clean %>%
  # group by variable to prepare for computing the median importance
  group_by(variable) %>%
  # calculate mean and median importance of each variable
  summarise(
    mean_importance = mean(importance, na.rm = TRUE),
    median_importance = median(importance, na.rm = TRUE)) %>%
  # Reorder with high to low median importance
  arrange(desc(median_importance))
print(tail(varImp_summary,5))
print(head(varImp_summary,5))

# Create boxplots of variable importances by removed variable
ggplot(varImp_clean, 
       aes(x = variable,             # Set x-axis to 'variable' 
           y = importance)) +        # Set y-axis to 'importance'
  # Add boxplots 
  geom_boxplot(fill = "#e1a486",    # Fill color 
               alpha = 1) +         # Opacity 
  # Apply a minimal theme 
  theme_minimal() +
  # Add plot labels
  labs(title = "Variable Importance Distributions",      # Title of the plot
       subtitle = "From Sensitivity Analysis",           # Subtitle
       y = "Importance",                                 # Label for y-axis
       y = "Variable",) +                                # Label for y-axis
  # format  labels and titles
  theme(axis.text.x = element_text(angle = 70,           # Rotate x-axis labels
                                   hjust = 1,            # align 
                                   size = 8),            # font size
        plot.title = element_text(hjust = 0.5),          # Center plot title
        plot.subtitle = element_text(hjust = 0.5))       # Center subtitle

################################################################################
# plot AUCs (sensitivity analysis)
################################################################################

# read in AUCs from sensitivity analysis
auc = read.csv("AUC_v17_sensitivityAnalysis.csv")

# extract the full model AUC from the dtaa frame
AUC_full <- auc %>%
  # filter for that row 
  filter(Removed == "full") %>%
  # extract the value
  pull(AUC)
print(AUC_full)

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
ggplot(df2, 
       aes(x = Removed,              # Set x-axis to 'Removed' variable
           y = AUC,                  # Set y-axis to 'AUC'
           group = 1)) +             # Group all points into one line
  # Add horizontal line for full model AUC
  geom_hline(yintercept = full_AUC, # place at full model AUC on y axis
             linetype = "dashed",   # line style
             color = "#e1a486",     # Line color
             size = 1) +            # Line thickness
  # Add AUC line
  geom_line(color = "#9c3400ff",    # Line color
            size = 1) +             # Line thickness
  # Add points to AUC line
  geom_point(size = 2.5,            # Point size
             color = "#9c3400ff") + # Point color
  # label full model AUC
  annotate("text", 
           x = 20,                                 # x-position 
           y = full_AUC + 0.0009,                  # y-position 
           label = "Full Model AUC = 0.845",       # Text 
           fontface = "bold",                      # bold
           color = "#e1a486",                      # Text color 
           hjust = 0) +                            # Left-align
  # Apply theme
  theme_minimal() +
  # Add plot labels
  labs(title = "AUC by Removed Variable",         # Title
       subtitle = "From Sensitivity Analysis",    # Subtitle
       x = "Removed Variable",                    # X-axis label
       y = "AUC") +                               # Y-axis label
  # Format labels and titles
  theme(axis.text.x = element_text(angle = 70,    # Rotate x-axis labels
                                   hjust = 1,     # Align labels
                                   size = 8),     # Font size
        plot.title = element_text(hjust = 0.5),   # Center title
        plot.subtitle = element_text(hjust = 0.5))# Center subtitle


