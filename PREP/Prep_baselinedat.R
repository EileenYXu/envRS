#### Imputing missing predictors in baseline data ####

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
init = mice(baseline, seed = 2404, maxit = 0)
pred = init$predictorMatrix
meth = init$method
meth["area_depriv"] = "2l.pmm"

# set site_id_l as the cluster
pred[,"src_subject_id"] = 0
pred[,"site_id_l"]= -2 # -2 denotes cluster
pred[,"gender_id"] = 0 # multicollinearitly between gender and birthsex
pred[,"birthsex"] = 0 # keeping gender only

# cluster must be integer
baseline$site_id_l = as.numeric(as.factor(baseline$site_id_l))

# multilevel imputation using 2l.pmm
imp = mice(baseline, seed = 2404, predictorMatrix = pred, method = meth, m=1, verbose = FALSE)

saveRDS(baseline, "G://users/eileen/ABCD/ABCD_Environmental_Risk/ABCDv5.1/DATA/dat_baseline.rds")