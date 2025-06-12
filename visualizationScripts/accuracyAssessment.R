################################################################################
# setup
################################################################################

# clear memory
rm(list = ls())

# packages
# ------------------------------------------------------------------------------

# install and load necessary packages
# install.packages("rstudioapi")
# install necessary packages
# install.packages("pROC")
# install.packages("data.table")
# install.packages("ggplot2")
library(pROC)
library(data.table)
library(ggplot2)
library(rstudioapi)

# working directory
# ------------------------------------------------------------------------------

# Get the directory of the currently running R script
script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)

# set as the working directory
setwd(script_dir)
# print(script_dir)

# change working directory to the folder with the testing point csv files 
# (exported from GEE)
setwd("../data/accuracyAssessment")
# getwd()

################################################################################
# plot ROC curves
################################################################################

# 2016-2023
# ------------------------------------------------------------------------------

# load in testing points
testing = read.csv("testingPts20162023_v17_11.csv")

# extract the true and predicted columns
true = testing$fireBinary
predicted = testing$classification

# create the ROC curve using the roc() function from the pROC package
roc_obj <- roc(true, predicted)
print(roc_obj)

# Calculate the AUC of the ROC curve
auc_value <- auc(roc_obj)
print(auc_value)

# Convert ROC object to data frame for ggplot
roc_df <- data.frame(
  specificity = rev(roc_obj$specificities),
  sensitivity = rev(roc_obj$sensitivities)
)

# Plot with ggplot2
# (set x-axis as FPR (1 - specificity), y-axis as TPR (sensitivity))
plot = ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +  
  geom_line(  # Add ROC curve
    color = "#9c3400ff",  # line color
    size = 1.2) +  # line thickness
  geom_abline(  # Add a diagonal dashed line as a reference
    linetype = "dashed",  # line type
    color = "#e1a486",  # lin ecolor
    size = 1) +  # line thickness
  coord_equal() +  # Force equal scaling on x and y axes
  labs(  # Add labels and titles
    title = "ROC Curve of Final Model for Testing Points from 2016-2023", # title  
    subtitle = paste("AUC =", round(auc_value, 3)),  # subtitle, round AUC
    x = "False Positive Rate (1 - Specificity)", # x-axis label
    y = "True Positive Rate (Sensitivity)") + # y-axis label
  theme_minimal() +  # Apply theme
  theme(
    text = element_text(size = 8),                      # Text size
    plot.title = element_text(hjust = 0.5, size = 16),   # title
    plot.subtitle = element_text(hjust = 0.5,size = 14), #subtitle
    axis.title.x = element_text(size = 12),              # X-axis label 
    axis.title.y = element_text(size = 12),              # Y-axis label 
    axis.text.x = element_text(size = 10),               # X-axis tick text 
    axis.text.y = element_text(size = 10)                # Y-axis tick text
  )

print(plot)

ggsave(filename = "ROC_20162023.png",        
       plot = plot,     
       bg = "white",
       width = 7, height = 6, dpi = 1000)

# 2024
# ------------------------------------------------------------------------------

# load in testing points
testing = read.csv("testingPts2024_v17_11.csv")

# extract the true and predicted columns
true = testing$fireBinary
predicted = testing$classification

# create the ROC curve using the roc() function from the pROC package
roc_obj <- roc(true, predicted)
print(roc_obj)

# Calculate the AUC of the ROC curve
auc_value <- auc(roc_obj)
print(auc_value)
print(roc_obj)

# Convert ROC object to data frame for ggplot
roc_df <- data.frame(
  specificity = rev(roc_obj$specificities),
  sensitivity = rev(roc_obj$sensitivities)
)

# Plot with ggplot2
plot = ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(
    color = "#9c3400ff", 
    size = 1.2) +
  geom_abline(
    linetype = "dashed", 
    color = "#e1a486", 
    size = 1) +
  coord_equal() +
  labs(
    title = "ROC Curve of Final Model for Testing Points 2024",
    subtitle = paste("AUC =", round(auc_value, 3)),
    x = "False Positive Rate (1 - Specificity)",
    y = "True Positive Rate (Sensitivity)") +
  theme_minimal() +
  theme(
    text = element_text(size = 8),                      
    plot.title = element_text(hjust = 0.5, size = 16),   
    plot.subtitle = element_text(hjust = 0.5,size = 14), 
    axis.title.x = element_text(size = 12),               
    axis.title.y = element_text(size = 12),               
    axis.text.x = element_text(size = 10),               
    axis.text.y = element_text(size = 10)                
  )

print(plot)

ggsave(filename = "ROC_2024.png",        
       plot = plot,     
       bg = "white",
       width = 7, height = 6, dpi = 1000)


