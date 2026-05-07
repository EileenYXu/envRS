######################################################
## Create train/test and resample for bootstrapping ##
######################################################

# Packages ----
library(tidyverse)
library(mice)
library(caret)

# Read in first imputed dataset ----
load("DATA/baseline_imp1.RData")

