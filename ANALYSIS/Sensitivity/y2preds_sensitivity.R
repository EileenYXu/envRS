#---------------------------------------------------------------#
# Sensitivity analysis with Y2 predictors predicting depression #
#---------------------------------------------------------------#

# complete case analysis

renv::load()
here::i_am("ANALYSIS/Sensitivity/y2preds_sensitivity.R")

## Packages ----
library(futurize)
library(tidyverse)
library(glmnet)
library(caret)
library(boot)

plan(multisession)
set.seed(211206)

dat = readRDS("DATA/dat_y2.rds") |> na.omit() |> filter(gender!="GNC") |> droplevels()

# dummy code gender to M = 0, F = 1, make interaction with bullying
dat = dat |> mutate(
  gender = case_when(gender=="M"~0, gender=="F"~1),
  bullying_gender = bullying_victim * gender)

### Y2 predictor variables and cbcl ----
preds = c("cbcl_dsm5_depress", "cbcl_internalising",
          "site_id_l", "total_su_days", "weightcontrol_ksads", 
          "witness_comm_violence", "death_loved_one", "witness_dv", "s_abuse", 
          "p_abuse", "emot_abuse", "serious_accident", "bkfs_fruit", "bkfs_veg",
          "bkfs_fiber", "sleep_hrs", "bmi", "needed_food", "income", "parent_ed",
          "gender", "area_depriv", "comm_safety", "discrimination", "days_acti",
          "bullying_victim", "bullying_gender", "cyberbullying", "chronotype",
          "life_events", "fam_conflict", "p_monitoring", "eff_control", 
          "p_depression", "interview_age")

# binary predictors to be dummy coded as 0/1 
binpreds = c("weightcontrol_ksads", "witness_comm_violence", "death_loved_one",
             "witness_dv", "s_abuse", "p_abuse", "emot_abuse",
             "serious_accident", "needed_food", "cyberbullying")

# numeric predictors to be centred and scaled
numpreds = c("site_id_l", "total_su_days", "bkfs_fruit", "bkfs_veg", "bkfs_fiber",
             "sleep_hrs", "bmi", "income", "parent_ed", "area_depriv", "comm_safety",
             "discrimination", "days_acti", "bullying_victim", "bullying_gender",
             "chronotype", "life_events", "fam_conflict", "p_monitoring",
             "eff_control", "p_depression", "interview_age")

## Setup for EN ----
## scale and centre numeric variables
dat = dat |> mutate(across(all_of(binpreds), ~ as.numeric(.x) - 1)) |> 
  mutate(across(all_of(numpreds),  ~ scale(as.numeric(.x)))) |> 
  mutate(across(cbcl_internalising:cbcl_dsm5_depress, ~ scale(as.numeric(.x))))

# split training/test
index = createDataPartition(dat$cbcl_dsm5_depress, p = 0.2, list = F, times = 1)

train = dat[-index,] |> 
  select(all_of(preds)) |> data.matrix()
test = dat[index,] |> 
  select(all_of(preds))

## assign each observation to a fold so that I can cross-validate alpha
foldid = sample(1:10, size = nrow(train), replace = TRUE)
alphas = seq(0, 1, by = 0.1) |> set_names()

## do not penalise site_id
pf = c(0, rep(1, 32))

## EN depression ----
## cross-validate alphas
depfits = alphas |> 
  map(\(i) cv.glmnet(x = train[,-c(1,2)], y = train[,1], foldid = foldid, 
                             type.measure = "deviance", alpha = i, 
                             penalty.factor = pf, intercept = FALSE)) |> futurize(seed=TRUE)

## get fit statistics for each alpha value in training data
names(depfits) = alphas

train_mse = depfits |> 
  imap(\(fit, i) 
       data.frame("alpha" = i,
                  "lambda.min" = fit$cvm[fit$lambda == fit$lambda.min],
                  "lambda.1se" = fit$cvm[fit$lambda == fit$lambda.1se])) |> 
  reduce(rbind)

# model with lowest MSE
train_mse[which.min(train_mse$lambda.min),] 
a = train_mse[which.min(train_mse$lambda.min),]$alpha
# min alpha = 1

best = depfits[[a]]
rm(depfits)

## Bootstrap coefficients for 95% CI ----
source("ANALYSIS/funs.R")

boot = boot(data = train[,-2], statistic = boot_glmnet, R = 2000,
            alpha = a, pf = pf, stype = "i", ycol = 1, intercept = F) |> 
  futurize(seed = TRUE)

## Get CIs ----
boot$t0 = coef(best, s = "lambda.min")
res = data.frame()

for (i in 1:length(boot$t0)) {
  ci = boot.ci(boot.out = boot, conf = 0.95, type = "basic", index = i)
  pred = rownames(boot$t0)[i]
  est = ci$t0
  lower = ci$basic[4]
  upper = ci$basic[5]
  res = rbind(res, c(pred, est, lower, upper)) |> setNames(c("Predictor", "Estimate", "Lower", "Upper"))
}

res = res |> mutate(
  Sig = case_when(
    Lower <= 0 & 0 <= Upper ~ "N",
    .default = "Y"
  )
)

# Test fit (all predictors) ----
coefs = as.numeric(res$Estimate) |> setNames(res$Predictor)
fit = test_fit(df = test, outcome = "cbcl_dsm5_depress", coefs = coefs[-1])
mse = mean(fit$residuals^2)
r2 = summary(fit)$r.squared

## Test fit using significant predictors only ----
sig = res |> filter(Sig=="Y" & Predictor!="(Intercept)")
sigcoef = as.numeric(sig$Estimate) |> set_names(sig$Predictor)

fit = test_fit(df = test, outcome = "cbcl_dsm5_depress", coefs = sigcoef)
mse_sig = mean(fit$residuals^2)
r2_sig = summary(fit)$r.squared

add = data.frame("Predictor"=c("Test R^2 all", "Test MSE all",
                               "Test R^2 sig", "Test MSE sig"),
                 "Estimate"=c(r2, mse, r2_sig, mse_sig),
                 "Lower"=rep(NA, 4), "Upper"=rep(NA, 4), "Sig"=rep(NA, 4))

res = rbind(res, add) |> mutate(
  Estimate = as.numeric(Estimate),
  Lower = as.numeric(Lower),
  Upper = as.numeric(Upper)
)

res

write.csv(res, file = "ANALYSIS/OUT/y2_depRS_sens.csv")

## EN internalising ----
## cross-validate alphas
intfits = alphas |> 
  map(\(i) cv.glmnet(x = train[,-c(1,2)], y = train[,2], foldid = foldid, 
                     type.measure = "deviance", alpha = i, 
                     penalty.factor = pf, intercept = FALSE)) |> futurize(seed=TRUE)

## get fit statistics for each alpha value in training data
names(intfits) = alphas

train_mse = intfits |> 
  imap(\(fit, i) 
       data.frame("alpha" = i,
                  "lambda.min" = fit$cvm[fit$lambda == fit$lambda.min],
                  "lambda.1se" = fit$cvm[fit$lambda == fit$lambda.1se])) |> 
  reduce(rbind)

# model with lowest MSE
train_mse[which.min(train_mse$lambda.min),] 
a = train_mse[which.min(train_mse$lambda.min),]$alpha
# min alpha = 1

best = intfits[[a]]
rm(intfits)

## Bootstrap coefficients for 95% CI ----
source("ANALYSIS/funs.R")

boot = boot(data = train[,-1], statistic = boot_glmnet, R = 2000,
            alpha = a, pf = pf, stype = "i", ycol = 1, intercept = F) |> 
  futurize(seed = TRUE)

## Get CIs ----
boot$t0 = coef(best, s = "lambda.min")
res = data.frame()

for (i in 1:length(boot$t0)) {
  ci = boot.ci(boot.out = boot, conf = 0.95, type = "basic", index = i)
  pred = rownames(boot$t0)[i]
  est = ci$t0
  lower = ci$basic[4]
  upper = ci$basic[5]
  res = rbind(res, c(pred, est, lower, upper)) |> setNames(c("Predictor", "Estimate", "Lower", "Upper"))
}

res = res |> mutate(
  Sig = case_when(
    Lower <= 0 & 0 <= Upper ~ "N",
    .default = "Y"
  )
)

# Test fit (all predictors) ----
coefs = as.numeric(res$Estimate) |> setNames(res$Predictor)
fit = test_fit(df = test, outcome = "cbcl_internalising", coefs = coefs[-1])
mse = mean(fit$residuals^2)
r2 = summary(fit)$r.squared

## Test fit using significant predictors only ----
sig = res |> filter(Sig=="Y" & Predictor!="(Intercept)")
sigcoef = as.numeric(sig$Estimate) |> set_names(sig$Predictor)

fit = test_fit(df = test, outcome = "cbcl_internalising", coefs = sigcoef)
mse_sig = mean(fit$residuals^2)
r2_sig = summary(fit)$r.squared

add = data.frame("Predictor"=c("Test R^2 all", "Test MSE all",
                               "Test R^2 sig", "Test MSE sig"),
                 "Estimate"=c(r2, mse, r2_sig, mse_sig),
                 "Lower"=rep(NA, 4), "Upper"=rep(NA, 4), "Sig"=rep(NA, 4))

res = rbind(res, add) |> mutate(
  Estimate = as.numeric(Estimate),
  Lower = as.numeric(Lower),
  Upper = as.numeric(Upper)
)

res

write.csv(res, file = "ANALYSIS/OUT/y2_intRS_sens.csv")
