#### Multiple imputation of missing data ####

#packages
library(tidyverse)
library(mice)
library(here)
library(miceadds)
here()

#read in all baseline data, remove columns that contain only missing values
dat = readRDS("DATA/predictors_unrelatedIDs.rds") |> filter(eventname=="baseline_year_1_arm_1") |> select_if(~ !all(is.na(.))) |> droplevels() 

#remove any variables with >20% missing data
baseline = dat |> select_if(~ sum(is.na(.))<0.2*nrow(dat))
names(dat)[(names(dat) %in% names(baseline))==F] #substance use measures removed

# also remove illicit (young age range and very few cases) and other unused variables
baseline = baseline |> select(-c(eventname, illicit, rel_family_id, rel_birth_id, interview_date, visit_type))

# remove empty levels
baseline = droplevels(baseline)

# check ICCs
numdat = baseline |> select(site_id_l, where(is.numeric))
out = c()
for (var in names(numdat)[-1]) {
  icc = ICC::ICCbare(x = site_id_l, y=var, data = numdat)
  out = c(out, icc)
}

data.frame(vars = names(numdat)[-1], ICC = out) #higher ICC for area deprivation, not overly high for any other predictor

#### impute missing baseline data ####
md.pattern(baseline) |> t()

# make predictor matrix and method vector
pred = make.predictorMatrix(baseline)
meth = make.method(baseline)

# set site_id_l as the cluster
pred[,"src_subject_id"] = 0
pred[,"site_id_l"]= -2 # -2 denotes cluster
pred[,"gender_id"] = 0 # multicollinearitly between gender and birthsex
pred[,"birthsex"] = 0 # keeping gender only

# impute
base_imp = mice(baseline, seed = 2404, predictorMatrix = pred, method = meth, m=3, print = FALSE)

# not happy with it currently, but I'm going to save what I've imputed anyway for now. I think weighting is the way
save(base_imp, file = "DATA/baseline_imputed.RData")
