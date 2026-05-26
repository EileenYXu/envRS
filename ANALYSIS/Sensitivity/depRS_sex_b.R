#######################################################################
## Predicting CBCL depression scores from environmental risk factors ##
#######################################################################

#### SENSITIVITY ANALYSIS WITH SEX ASSIGNED AT BIRTH ####

renv::load()
here::i_am("ANALYSIS/Sensitivity/depRS_sex_b.R")

# Packages ----
library(futurize)

plan(multisession, workers = 3)

library(tidyverse)
library(miselect)
library(mice)
library(boot)
set.seed(211206)

load("DATA/baseline_imputed.RData")
load("DATA/idlist_sens.RData")

dfs = lapply(1:50, function (i) complete(base_imp, action = i)) |> 
  futurize()

# depRS predictor variables ----
preds = c("site_id_l", "tobacco_puff", "weightcontrol_ksads", 
          "witness_comm_violence", "death_loved_one", "witness_dv", "s_abuse", 
          "p_abuse", "emot_abuse", "serious_accident", "sleep_hrs", "bmi", 
          "needed_food", "income", "parent_ed", "birthsex", "area_depriv", "comm_safety", 
          "days_acti", "fam_conflict", "p_monitoring", "p_acceptance", 
          "p_depression", "interview_age")

# binary predictors to be dummy coded as 0/1 
# birthsex recoded as M=0, F=1 below
binpreds = c("tobacco_puff", "weightcontrol_ksads", "witness_comm_violence",
             "death_loved_one", "witness_dv", "s_abuse", "p_abuse", "emot_abuse",
             "serious_accident", "needed_food")

# numeric predictors to be centred and scaled
numpreds = c("site_id_l", "sleep_hrs", "bmi", "income", "parent_ed", "area_depriv", 
             "comm_safety", "days_acti", "fam_conflict", "p_monitoring", 
             "p_acceptance", "p_depression", "interview_age")

# generate list of imputed predictor matrices (x) and CBCL depression (y2, base)
# recode, centre and scale
x = list()
y2 = list()
base = list()
scaled = list()

for (i in 1:length(dfs)) {
  
  df = dfs[[i]] |> filter(birthsex != "Intersex") |> 
    droplevels() |> mutate(birthsex = case_when(birthsex=="M"~0, birthsex=="F"~1)) |> 
    mutate(across(all_of(binpreds), ~ as.numeric(.x) - 1)) |> 
    mutate(across(all_of(numpreds),  ~ scale(as.numeric(.x)))) |> 
    mutate(across(cbcl_dsm5_depress:cbcl_dsm5_depress_y2, ~ scale(as.numeric(.x))))
  
  x[[i]] = df |> filter(src_subject_id %in% train_ids_sens) |> 
    select(all_of(preds)) |> data.matrix()
  
  y2[[i]] = df |> filter(src_subject_id %in% train_ids_sens) |> 
    pull(cbcl_dsm5_depress_y2) |> as.vector()
  base[[i]] = df |> filter(src_subject_id %in% train_ids_sens) |> 
    pull(cbcl_dsm5_depress) |> as.vector()
  
  scaled[[i]] = df |> filter(src_subject_id %in% test_ids_sens)
}

# fit EN models ----

# weight each observation by proportion of missing data
ogdat = base_imp$data |> filter(src_subject_id %in% train_ids_sens)

misweights = 1 - rowMeans(is.na(ogdat))

# do not penalise site_id_l
pf = c(0, rep(1, 23))

# cross-validate over alphas
alphas = seq(0, 1, by = 0.1)

# no adaptive weights (i.e. each of 24 predictors is weighted the same)
adwt = rep(1, 24)

####### Fit EN for Baseline CBCL #############

# Cross-validate alpha and lambda ----
bfit = cv.saenet(x, base, pf = pf, alpha = alphas, weights = misweights, 
                 nfolds = 10, adWeight = adwt, family = "gaussian")

b_l = bfit$lambda.min
b_a = bfit$alpha.min
bcoef = coef(bfit)

b_a
b_l

# Bootstrap coefficients ----
source("ANALYSIS/boot_saenet.R")

b_boot = boot(data = x[[1]], statistic = boot_saenet, R = 2000,
              pred = x, out = base, pf = pf, a = b_a, l = b_l, wt = misweights,
              adwt = adwt) |> futurize(seed=TRUE)

# Get CIs for each estimate ----
b_boot$t0 = bcoef
b_res = data.frame()

for (i in 1:length(bcoef)) {
  ci = boot.ci(boot.out = b_boot, conf = 0.95, type = "basic", index = i)
  pred = names(ci$t0)
  est = ci$t0
  lower = ci$basic[4]
  upper = ci$basic[5]
  b_res = rbind(b_res, c(pred, est, lower, upper)) |> setNames(c("Predictor", "Estimate", "Lower", "Upper"))
}

b_res = b_res |> mutate(
  Sig = case_when(
    Lower <= 0 & 0 <= Upper ~ "N",
    .default = "Y"
  )
)

# Test fit (all predictors) ----
fitlist = scaled |> 
  map(\(df) test_fit(df = df, outcome = "cbcl_dsm5_depress",
                     coefs = bcoef[-1])) |> futurize(seed=T)

mse = fitlist |> map_dbl(\(fit) mean(fit$residuals^2)) |> mean()
r2 = pool.r.squared(as.mira(fitlist))

## Test fit using significant predictors only ----
sig = b_res |> filter(Sig=="Y" & Predictor!="(Intercept)") |> 
  select(Predictor, Estimate)
sigcoef = as.numeric(sig$Estimate) |> set_names(sig$Predictor)

fitlist = scaled |> 
  map(\(df) test_fit(df = df, outcome = "cbcl_dsm5_depress",
                     coefs = sigcoef)) |> futurize(seed=T)

mse_sig = fitlist |> map_dbl(\(fit) mean(fit$residuals^2)) |> mean()
r2_sig = pool.r.squared(as.mira(fitlist))

add = data.frame("Predictor"=c("Test R^2 all", "Test MSE all",
                               "Test R^2 sig", "Test MSE sig"),
                 "Estimate"=c(r2[1,1], mse, r2_sig[1,1], mse_sig),
                 "Lower" = c(r2[1,2], NA, r2_sig[1,2], NA),
                 "Upper" = c(r2[1,3], NA, r2_sig[1,3], NA),
                 "Sig" = rep(NA, 4))

b_res = rbind(b_res, add) |> mutate(
  Estimate = as.numeric(Estimate),
  Lower = as.numeric(Lower),
  Upper = as.numeric(Upper)
)

write.csv(b_res, file = "ANALYSIS/OUT/depRS_sex_base.csv")
