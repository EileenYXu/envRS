#######################################################################
## Predicting CBCL depression scores from environmental risk factors ##
#######################################################################

# Packages ----
library(tidyverse)
library(miselect)
set.seed(211206)

load("DATA/baseline_imputed.RData")
load("DATA/idlist_main.RData")

dfs = lapply(1:50, function (i) complete(base_imp, action = i))

# depRS predictor variables ----
preds = c("site_id_l", "tobacco_puff", "weightcontrol_ksads", 
          "witness_comm_violence", "death_loved_one", "witness_dv", "s_abuse", 
          "p_abuse", "emot_abuse", "serious_accident", "sleep_hrs", "bmi", 
          "needed_food", "income", "gender", "area_depriv", "comm_safety", 
          "days_acti", "fam_conflict", "p_monitoring", "p_acceptance", 
          "p_depression", "interview_age")

# binary predictors to be dummy coded as 0/1 
# gender recoded as M=0, F=1 below
binpreds = c("tobacco_puff", "weightcontrol_ksads", "witness_comm_violence",
             "death_loved_one", "witness_dv", "s_abuse", "p_abuse", "emot_abuse",
             "serious_accident", "needed_food")

# numeric predictors to be centred and scaled
numpreds = c("site_id_l", "sleep_hrs", "bmi", "income", "area_depriv", 
             "comm_safety", "days_acti", "fam_conflict", "p_monitoring", 
             "p_acceptance", "p_depression", "interview_age")

# generate list of imputed predictor matrices (x) and CBCL depression (y2, base)
# recode, centre and scale
x = list()
y2 = list()
base = list()

for (i in 1:50) {
  
  df = dfs[[i]] |> filter(src_subject_id %in% train_ids) |> 
    droplevels() |> mutate(gender = case_when(gender=="M"~0, gender=="F"~1))
  
  x[[i]] = df |> select(all_of(preds)) |> 
    mutate(across(all_of(binpreds), ~ as.numeric(.x) - 1)) |> 
    mutate(across(all_of(numpreds), ~ scale(as.numeric(.x)))) |> 
    data.matrix()
  
  y2[[i]] = df$cbcl_dsm5_depress_y2
  base[[i]] = df$cbcl_dsm5_depress
  
}

# fit EN models ----

# weight each observation by proportion of missing data
weights = 1 - rowMeans(is.na(
  base_imp$data[which(base_imp$data$src_subject_id %in% train_ids),]))

# do not penalise site_id_l
pf = c(0, rep(1, 22))

# cross-validate over alphas
alphas = seq(0, 1, by = 0.1)

# no adaptive weights
adwt = rep(1, 23)

y2fit = cv.saenet(x, y2, pf = pf, alpha = alphas, weights = weights, 
                  nfolds = 10, adWeight = adwt, family = "gaussian")
basefit = cv.saenet(x, base, pf = pf, alpha = alphas, weights = weights, 
                    nfolds = 10, adWeight = adwt, family = "gaussian")

