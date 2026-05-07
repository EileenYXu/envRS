######################################################
## Create train/test and resample for bootstrapping ##
######################################################

# Packages ----
library(tidyverse)
library(mice)
library(caret)
set.seed(705)

# Read in first imputed dataset ----
load("DATA/baseline_imputed.RData")
imp1 = complete(base_imp, action = 1)

# Main analyses using gender identity as predictor ----
# individuals with gender = GNC have to be excluded unfortunately - the very low frequency is likely to lead to zero-variance problems
dat = imp1 |> filter(gender!="GNC") |> droplevels()

## Split train/test ids ----
# for numeric y, createDataPartition splits sample into quantiles and samples within each subgroup
t_index = createDataPartition(y = dat$cbcl_dsm5_depress_y2, times = 1, p = 0.8, list = FALSE, groups = 10)

train_ids = dat$src_subject_id[t_index]
test_ids = dat$src_subject_id[-t_index]
save(train_ids, test_ids, file="DATA/idlist_main.RData")

# Sensitivity analysis using assigned sex at birth as a predictor ----
# Exclude individuals with birthsex = Intersex due to the same zero-variance problem
sensdat = imp1 |> filter(birthsex!="Intersex") |> droplevels()

## Split train/test ids ----
# for numeric y, createDataPartition splits sample into quantiles and samples within each subgroup
t_index = createDataPartition(y = sensdat$cbcl_dsm5_depress_y2, times = 1, p = 0.8, list = FALSE, groups = 10)

train_ids_sens = sensdat$src_subject_id[t_index]
test_ids_sens = sensdat$src_subject_id[-t_index]

save(train_ids_sens, test_ids_sens, file="DATA/idlist_sens.RData")

# Code to create bootstrap samples ----
# saving this for later use - it's too large to justify making in advance and saving...
traindat = dat[which(dat$src_subject_id %in% train_ids),]

samps = createResample(y = traindat$cbcl_dsm5_depress_y2, times = 2000)

for (samp in 1:2000) {
  index = samps[[samp]]
  ids = train_ids[index]
  idlist[[paste("Resample", samp, collapse = "_")]] = ids
}


