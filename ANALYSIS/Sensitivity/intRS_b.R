##########################################################################
## Predicting CBCL internalising scores from environmental risk factors ##
##########################################################################

renv::load()
here::i_am("ANALYSIS/Sensitivity/intRS_b.R")

# Packages ----
library(futurize)
library(tidyverse)
library(mice)
library(boot)
library(glmnet)

plan(multisession)
set.seed(211206)

load("DATA/baseline_imputed.RData")
load("DATA/idlist_main.RData")

dfs = lapply(1:50, function (i) complete(base_imp, action = i)) |> 
  futurize(seed=TRUE)

# Set up for EN ----
# binary predictors to be dummy coded as 0/1, gender recoded as M=0, F=1 below
binpreds = c("tobacco_puff", "weightcontrol_ksads", "witness_comm_violence",
             "death_loved_one", "witness_dv", "s_abuse", "p_abuse", "emot_abuse",
             "serious_accident", "needed_food")

# numeric predictors to be centred and scaled
numpreds = c("site_id_l", "sleep_hrs", "bmi", "income", "parent_ed", 
             "area_depriv", "comm_safety", "days_acti", "fam_conflict", 
             "p_monitoring", "p_acceptance", "p_depression", "interview_age")

# variables to keep for x 
xvars = c("src_subject_id", numpreds, binpreds, "gender")

# weight each ppt by % complete / m imputed datasets
misswts = rowMeans(!is.na(base_imp$data[,c(xvars, "cbcl_internalising")])) /50

# recode predictors, scale cbcl, add weights
dfs = dfs |> map(\(df) mutate(df,
                              across(all_of(binpreds), ~ as.numeric(.x) - 1),
                              across(all_of(numpreds),  ~ scale(as.numeric(.x))),
                              gender = case_when(gender=="M"~0, gender=="F"~1),
                              cbcl_internalising = scale(as.numeric(cbcl_internalising)),
                              misswt = misswts
)) |> futurize(seed=TRUE)

## stack training datasets ----
traindfs = dfs |> map(\(df) filter(df, src_subject_id %in% train_ids)) |> 
  futurize(seed=TRUE)
train = reduce(traindfs, rbind) |> droplevels()
train_x = train |> select(all_of(xvars)) |> data.matrix()
train_y = train |> select(cbcl_internalising) |> data.matrix()
trainwts = train |> pull(misswt)

## test data ----
testdfs = dfs |> map(\(df) filter(df, src_subject_id %in% test_ids)) |> 
  futurize(seed=TRUE)

# clean up
rm(dfs, traindfs, train, misswts, binpreds, numpreds)
gc()

# fit EN model with 10-fold cross-validation ----

# make foldid to cross-validate alpha 
foldid = sample(1:10, size = length(train_ids), replace = TRUE) |> rep(times=50)

# do not penalise site_id_l
pf = c(0, rep(1, 23))

# cross-validate over alphas
alphas = seq(0, 1, by = 0.1)

depfits = alphas |> 
  map(\(a) cv.glmnet(x = train_x[,-1], y = train_y, foldid = foldid, 
                     type.measure = "mse", alpha = a, 
                     penalty.factor = pf, nfolds = 10, 
                     weights = trainwts, intercept = FALSE)) |> 
  futurize(seed=TRUE)

# get alpha with lowest mse
names(depfits) = alphas

train_mse = depfits |> imap(\(fit, i)
                            data.frame("alpha" = i, 
                                       "lambda.min" = fit$cvm[fit$lambda == fit$lambda.min],
                                       "lambda.1se" = fit$cvm[fit$lambda == fit$lambda.1se])) |> 
  reduce(rbind)

# model with lowest MSE
train_mse[which.min(train_mse$lambda.min),] 
a = train_mse[which.min(train_mse$lambda.min),]$alpha

# most penalised model with lowest MSE
train_mse[which.min(train_mse$lambda.1se),]

best = depfits[[a]]
rm(depfits)

# Bootstrap coefficients ----
source("ANALYSIS/funs.R")

# run boot() on unique(x[,1]) for consistent resampling across datasets
boot = boot(data = unique(train_x[,1]), statistic = miboot_glmnet, R = 2000,
            xmat = train_x, ymat = train_y, pf = pf, alpha = as.numeric(a), wt = trainwts,
            intercept = FALSE) |> 
  futurize(seed=TRUE)

# Get CIs for each estimate ----
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

fitlist = testdfs |> 
  map(\(df) test_fit(df = df, outcome = "cbcl_internalising",
                     coefs = coefs[-1]))

mse = fitlist |> map_dbl(\(fit) mean(fit$residuals^2)) |> mean()
r2 = pool.r.squared(as.mira(fitlist))

## Test fit using significant predictors only ----
sig = res |> filter(Sig=="Y" & Predictor!="(Intercept)") |> 
  select(Predictor, Estimate)
sigcoef = as.numeric(sig$Estimate) |> set_names(sig$Predictor)

fitlist = testdfs |> 
  map(\(df) test_fit(df = df, outcome = "cbcl_internalising",
                     coefs = sigcoef)) |> futurize(seed=T)

mse_sig = fitlist |> map_dbl(\(fit) mean(fit$residuals^2)) |> mean()
r2_sig = pool.r.squared(as.mira(fitlist))

add = data.frame("Predictor"=c("Test R^2 all", "Test MSE all",
                               "Test R^2 sig", "Test MSE sig"),
                 "Estimate"=c(r2[1,1], mse, r2_sig[1,1], mse_sig),
                 "Lower" = c(r2[1,2], NA, r2_sig[1,2], NA),
                 "Upper" = c(r2[1,3], NA, r2_sig[1,3], NA),
                 "Sig" = rep(NA, 4))

res = rbind(res, add) |> mutate(
  Estimate = as.numeric(Estimate),
  Lower = as.numeric(Lower),
  Upper = as.numeric(Upper)
)

res

write.csv(res, file = "ANALYSIS/OUT/intRS_base.csv")
