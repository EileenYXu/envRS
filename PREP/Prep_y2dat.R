## ----setup, include=FALSE------------------------------------------------------------------
knitr::opts_chunk$set(echo = F, message = F, results = "asis", warning = F)
library(tidyverse)
library(kableExtra)
options(knitr.kable.NA = '')
library(summarytools)


## ------------------------------------------------------------------------------------------
dat = readRDS("G://users/eileen/ABCD/ABCD_Environmental_Risk/ABCDv5.1/DATA/predictors_unrelatedIDs.rds")
y2 = dat |> filter(eventname=="2_year_follow_up_y_arm_1") |> select_if(~ !all(is.na(.))) |> droplevels()


## ------------------------------------------------------------------------------------------
y2 |> select_if(~ is.numeric(.)) |> summary(.) |> kbl() |> kable_styling()


## ------------------------------------------------------------------------------------------
y2 |> select_if(~ is.factor(.)) |> summary() |> kbl() |> kable_styling()


## ------------------------------------------------------------------------------------------
y2 = y2 |> mutate(
  tlfb_cal_scr_num_events = replace_na(tlfb_cal_scr_num_events, 0)
)

y2 = y2 |> select_if(~ sum(is.na(.))<0.2*nrow(y2))
y2 = y2 |> select(-c(eventname, illicit, site_id_l, rel_birth_id, interview_date, visit_type))

names(y2) = c("src_subject_id", "tlfb_use_days", "weightcontrol_ksads", "witness_comm_violence", "death_loved_one", "witness_dv", "s_abuse", "p_abuse", "emot_abuse", "serious_accident", "bkfs_fruit", "bkfs_veg", "bkfs_fiber", "sleep_hrs", "bmi", "needed_food", "income", "parent_ed", "birthsex","gender_id", "gender", "race_ethnicity", "area_depriv", "comm_safety", "discrimination", "days_active", "bullying_victim", "cyberbullying", "chronotype", "life_events", "fam_conflict", "p_monitoring", "eff_control", "p_depression", "agemths", "cbcl_internalising", "cbcl_dsm5_depress")

y2 = droplevels(y2)

# remove cbcl missing data
y2 = y2 |> filter(!is.na(cbcl_internalising))
sapply(y2, function(x) sum(is.na(x))) |> kbl(col.names = c("", "N missing")) |> kable_styling() |> scroll_box()


## ------------------------------------------------------------------------------------------
saveRDS(y2, "G://users/eileen/ABCD/ABCD_Environmental_Risk/ABCDv5.1/DATA/dat_y2.rds")

