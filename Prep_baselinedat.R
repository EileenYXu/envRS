## ----setup, include=FALSE------------------------------------------------------------------
knitr::opts_chunk$set(echo = F, message = F, results = "asis", warning = F)
library(tidyverse)
library(kableExtra)
options(knitr.kable.NA = '')
library(summarytools)


## ------------------------------------------------------------------------------------------
dat = readRDS("G://users/eileen/ABCD/ABCD_Environmental_Risk/ABCDv5.1/DATA/predictors_unrelatedIDs.rds")
baseline = dat |> filter(eventname=="baseline_year_1_arm_1") |> select_if(~ !all(is.na(.))) |> droplevels()


## ------------------------------------------------------------------------------------------
## remove any variables with >20% missing data?
baseline = baseline |> select_if(~ sum(is.na(.))<0.2*nrow(baseline))

baseline = baseline |> select(-c(eventname, illicit, site_id_l, rel_family_id, rel_birth_id, interview_date, visit_type))

names(baseline) = c("src_subject_id", "tobacco_puff", "weightcontrol_ksads",
                    "witness_comm_violence", "death_loved_one",
                    "witness_dv", "s_abuse", "p_abuse", "emot_abuse",
                    "serious_accident", "sleep_hrs", "bmi", "needed_food",
                    "income", "parent_ed", "birthsex", "gender_id", "gender",
                    "race_ethnicity","area_depriv", "comm_safety",
                    "days_active", "fam_conflict", "p_monitoring", "p_acceptance",
                    "p_depression", "agemths", "cbcl_internalising", "cbcl_dsm5_depress")

# remove empty levels
baseline = droplevels(baseline)

# remove cbcl missing data
baseline = baseline |> filter(!is.na(cbcl_internalising))

sapply(baseline, function(x) sum(is.na(x))) |> kbl(col.names = c("", "N missing")) |> kable_styling() |> scroll_box()


## ------------------------------------------------------------------------------------------
## remove missing data
baseline = baseline |> na.omit() |> droplevels()

saveRDS(baseline, "G://users/eileen/ABCD/ABCD_Environmental_Risk/ABCDv5.1/DATA/dat_baseline.rds")

