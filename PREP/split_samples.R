######################################################
## Create train/test and resample for bootstrapping ##
######################################################

# Packages ----
library(tidyverse)
library(mice)
library(caret)

# Read in first imputed dataset ----
load("DATA/baseline_imputed.RData")

# Main analyses using gender identity as predictor ----
# individuals with gender = GNC have to be excluded unfortunately - the very low frequency is likely to lead to zero-variance problems
dat = imp1 |> filter(gender!="GNC") |> droplevels()

# for numeric y, createDataPartition splits sample into quantiles and samples within each subgroup
t_index = createDataPartition(y = dat$cbcl_dsm5_depress_y2, times = 1, p = 0.8, list = FALSE, groups = 10)

train_ids = dat$src_subject_id[t_index]
test_ids = dat$src_subject_id[-t_index]

