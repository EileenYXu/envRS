#### Imputing missing predictors in baseline data ####

#packages
library(tidyverse)

#read in all baseline data, remove columns that contain only missing values
dat = readRDS("DATA/predictors_unrelatedIDs.rds") |> filter(eventname=="baseline_year_1_arm_1") |> select_if(~ !all(is.na(.))) |> droplevels() 

#remove any variables with >20% missing data
baseline = dat |> select_if(~ sum(is.na(.))<0.2*nrow(dat))
names(dat)[(names(dat) %in% names(baseline))==F] #substance use measures removed

# also remove illicit (young age range and very few cases) and other unused variables
baseline = baseline |> select(-c(eventname, illicit, rel_family_id, rel_birth_id, interview_date, visit_type))

# remove empty levels
baseline = droplevels(baseline)

#saveRDS(baseline, "G://users/eileen/ABCD/ABCD_Environmental_Risk/ABCDv5.1/DATA/dat_baseline.rds")