################################################################################
# setup
################################################################################

# clear memory
rm(list = ls())

# packages
# ------------------------------------------------------------------------------

# install and load necessary packages
# install.packages("dplyr")
# install.packages("car")
# install.packages("tidyverse")
# install.packages("ggcorrplot")
# install.packages("corrplot")
# install.packages("GGally")
# install.packages("lares")
library(car)
library(tidyverse)
library(ggcorrplot)
library(dplyr)
library(corrplot)
library(GGally)
library(lares)

# working directory
# ------------------------------------------------------------------------------

# Get the directory of the currently running R script
script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)

# set as the working directory
setwd(script_dir)
# print(script_dir)

# change working directory to the folder with the testing point csv files 
# (exported from GEE)
setwd("../data/multicollinearity")
# getwd()

# clean labels
# ------------------------------------------------------------------------------

# Create a named vector of nice labels
label_clean <- c(
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

################################################################################
# load and clean data
################################################################################

# import reference points
refPts = read.csv("referencePts20162023_v17.csv")
# print(names(refPts))

# clean data frame
refPts_clean = refPts %>% 
  # remove unnecessary columns
  select(-system.index, -.geo, -year, -fireBinary, -random) %>% 
  # remove unnecessary rows (variables not used in the model at all)
  select(-VVsentinel1Cdry,
         -HHpalsarLdry,
         -VVsentinel1Cwet,
         -HHpalsarLwet,
         -VVsentinel1CdiffWetDry,
         -blue,
         -green,
         -red,
         -nir,
         -temp,
         -swir1,
         -swir2,
         -NDVI,
          -temperatureMin,
          -temperatureMean,
          -canopyHeight,
         -distanceSwampForest,
          -distanceMangroveForest,
          -distanceBeachForest,
          -distanceInundatedForest,
          -distanceMoistEvergreenForest,
          )
# print(names(refPts_clean))

################################################################################
# correlation matrix
################################################################################

# calculate correlation between all variables
corr <- round(cor(refPts_clean), 1) 

# change the varibale names to the clean names
rownames(corr) <- label_clean[rownames(corr)]
colnames(corr) <- label_clean[colnames(corr)]
# print(rownames(corr))
# print(colnames(corr))

# create a correlation plot
ggcorrplot(corr,                     # Plot correlation matrix
           hc.method = "median",     # Use median method for clustering
           hc.order = TRUE,          # Order variables by cluster
           type = "lower",           # Show only lower triangle
           outline.col = "black",    # Add black border to boxes
           lab = TRUE,               # Show correlation numbers
           lab_size = 1.5,           # Set size of correlation numbers
           legend.title = "Correlation"  # Title for color legend
  ) +
  theme(
    axis.text.x = element_text(size = 5,         # x variable label size
                               color = 'black'), # x variable label color
    axis.text.y = element_text(size = 5,         # y variable label size
                               color = 'black'), # y variable label color
    text = element_text(size = 8),               # text size
    legend.position = c(.2, .75),                # Move legend to top left
    legend.key = element_rect(fill = "white"),   # legend background
    plot.title = element_text(hjust = 0.5)       # Center title
  ) +
  scale_y_discrete(position = 'right') +         # Move y-axis labels to right
  guides(fill = guide_colorbar(ticks.colour = NA)) +  # remove ticks from color bar
  labs(
    title = "Predictor Variable Correlations",  # Title of plot
    x = "Variable",                             # x-axis labels
    y = "Variable"                              # y-axis labels
  )

################################################################################
# correlation bar chart
################################################################################

# function from lares package
# ------------------------------------------------------------------------------

# create bar chart 
corr_cross(refPts_clean, 
           max_pvalue = 0.10, # display only significant correlations
           top = 20 # display top 20 couples of correlations
)

# manual
# ------------------------------------------------------------------------------

# Compute Correlation Matrix
corMatrix <- refPts_clean %>%
  # select numeric values
  select(where(is.numeric)) %>% 
  # use only complete pairs
  cor(use = "pairwise.complete.obs")
# print(corMatrix)

# create a correlation dataframe for plotting
corMatrix_clean <- as.data.frame(as.table(corMatrix)) %>%
  # remove self correlations
  filter(Var1 != Var2) %>%  
  # add a column of absolute values of correlations
  mutate(absCorr = abs(Freq)) %>%
  # rename correlation column
  rename(corr = Freq) %>% 
  # arrange from high to low for distinct function
  arrange(desc(absCorr)) %>%
  # remove duplicate combinations
  distinct(pmin(Var1, Var2), 
           pmax(Var1, Var2), 
           .keep_all = TRUE) %>%
  # get the correlations above 0.50
  filter(absCorr > 0.50) %>%   
  # get clean variable labels
  mutate(Var1 = label_clean[as.character(Var1)]) %>% 
  mutate(Var2 = label_clean[as.character(Var2)]) %>%
  # create a new column with variable pairs
  mutate(pair = paste(Var1, "+", Var2)) %>%
  # arrange based on correlation for plotting order
  arrange(absCorr) %>%
  # make pairs a factor to maintain order in plot
  mutate(pair = factor(pair, levels = pair))
# print(corMatrix_clean)

# plot correlation coefficients as horizontal bar chart
ggplot(corMatrix_clean, 
       aes(x = pair, 
           y = absCorr,            # y axis as absolute value of correlation 
           fill = corr)) +          # fill color by actual correlation
  geom_col() +                                  # Add bars to the plot
  geom_text(aes(label = round(corr, 2)),        # Add correlation values as labels
            hjust = -0.1,                       # place just outside bars
            size = 3.5) +                       # text size
  coord_flip() +                                # Flip axes for horizontal bars
  theme_minimal() +                             # minimal theme
  scale_y_continuous(expand = c(0, 0),          # Remove extra space on y-axis
                     limits = c(0, max(corMatrix_clean$absCorr) + 0.1)) +  # Set y-axis limits
  scale_fill_gradient2(          # fill color gradient for bars
    low = "blue",                
    mid = "white",               
    high = "red",                  
    midpoint = 0,                # Set midpoint of gradient at 0
    limits = c(-1, 1),           # Correlation range
    name = "Correlation"         # Legend title
  ) +
  labs(
    title = "Strongest Variable Correlations",   # Plot title
    x = "Variable Pair",                         # x-axis label
    y = "Correlation Magnitude"                  # y-axis label
  ) +
  theme(
    axis.text.y = element_text(size = 8),        # y-axis labels text size
    plot.title = element_text(hjust = 0.5),      # Center the title
    legend.position = "right"                    # Show legend on the right
  )

################################################################################
# VIF
################################################################################

# clean data frame, keeping fireBinary in
refPts_clean = refPts %>% 
  # remove unnecessary columns
  select(-system.index, -.geo, -year, -random) %>% 
  # remove unnecessary rows (variables not used in the model at all)
  select(-VVsentinel1Cdry,
         -HHpalsarLdry,
         -VVsentinel1Cwet,
         -HHpalsarLwet,
         -VVsentinel1CdiffWetDry,
         -blue,
         -green,
         -red,
         -nir,
         -temp,
         -swir1,
         -swir2,
         -NDVI,
         -temperatureMin,
         -temperatureMean,
         -canopyHeight,
         -distanceSwampForest,
         -distanceMangroveForest,
         -distanceBeachForest,
         -distanceInundatedForest,
         -distanceMoistEvergreenForest,
  )
print(names(refPts_clean))

# Fit a linear model with all predictors
model <- lm(fireBinary ~ ., data = refPts_clean)

# Calculate VIFs
vif <- vif(model)
# print(vif_values)

# clean up VIF results
vif_clean <- data.frame(Variable = names(vif),  
                        VIF = vif) %>% 
  # arrange in descending order
  arrange(desc(VIF)) %>% 
  # replace variable names with nice labels
  mutate(NiceLabel = label_clean[as.character(Variable)]) %>% 
  # make variable names  factors to maintain order
  mutate(NiceLabel = factor(NiceLabel, levels = rev(NiceLabel))) 
# print(vif_clean)

# Plot VIFs as bar plot
ggplot(vif_clean,                
       aes(x = NiceLabel,               # labels on x-axis
           y = VIF)) +                  # VIF on y-axis v
  geom_col(fill = "#9c3400ff") +        # bar color
  geom_text(aes(label = round(VIF, 1)), # Add VIFs as labels on bars
            hjust = -0.1,               # text location
            size = 3.5) +               # text size
  coord_flip() +                        # Flip coordinates for horizontal bars
  theme_minimal() +                     #  theme
  scale_y_continuous(expand = c(0, 0), 
                     limits = c(0, max(vif_clean$VIF) + 1)) +  # y-axis range
  labs(
    title = "Variance Inflation Factors (VIF)", # title
    x = "Variable",                             # x-axis label
    y = "VIF"                                   # y-axis label
  ) +
  theme(
    axis.text.y = element_text(size = 10),      # size y-axis labels
    plot.title = element_text(hjust = 0.5)      # Center plot title
  )

