###################################################################
## Sensitivity analysis with Y2 predictors predicting depression ##
###################################################################

# using glmnet, not imputing

renv::load()
here::i_am("ANALYSIS/Sensitivity/y2preds_depRS.R")
set.seed(211206)

# Packages ----
library(futurize)
plan(multisession)

library(tidyverse)
library(glmnet)
library(caret)
library(boot)

dat = readRDS("DATA/dat_y2.rds") |> na.omit() |> filter(gender!="GNC") |> droplevels()

# dummy code gender to M = 0, F = 1, make interaction with bullying
dat = dat |> mutate(
  gender = case_when(gender=="M"~0, gender=="F"~1),
  bullying_gender = bullying_victim * gender)

# depRS predictor variables and cbcl ----
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

## scale and centre numeric variables
dat = dat |> mutate(across(all_of(binpreds), ~ as.numeric(.x) - 1)) |> 
  mutate(across(all_of(numpreds),  ~ scale(as.numeric(.x)))) |> 
  mutate(across(cbcl_internalising:cbcl_dsm5_depress, ~ scale(as.numeric(.x))))

# split training/test
index = createDataPartition(dat$cbcl_dsm5_depress, p = 0.2, list = F, times = 1)

train = dat[-index,] |> 
  select(all_of(preds)) |> data.matrix()
test = dat[index,] |> 
  select(all_of(preds)) |> data.matrix()

## Fit EN ----

## assign each observation to a fold so that I can cross-validate alpha
foldid = sample(1:10, size = nrow(train), replace = TRUE)
alphas = seq(0, 1, by = 0.1)

## do not penalise site_id
pf = c(0, rep(1, 32))

## use for loop to cross-validate alphas
depfits = list()
for(i in 1:length(alphas)) {
  depfits[[paste0("alpha_", alphas[i])]] = cv.glmnet(
    x = train[,-c(1,2)], y = train[,1], foldid = foldid, type.measure = "deviance",
    alpha = alphas[i], penalty.factor = pf)
}

## use for loop to get fit statistics for each alpha value in training data
trainfits = list()
for (i in 1:length(depfits)) {
  trainfits[[paste0("alpha_", alphas[i])]] = assess.glmnet(
    depfits[[i]], newx = train[,-c(1,2)], newy = train[,1], s = "lambda.min")
}

train_mse = sapply(trainfits, function(x) x$mse[["lambda.min"]])
rownames(data.frame(train_mse))[which.min(train_mse)]
# min alpha = 0
# train MSE = 0.697499  
train_mse["alpha_0"]

# coefficients
coefs = coef(depfits$alpha_0) |> as.matrix()

## test data MSE = 0.7013873   
assess.glmnet(depfits$alpha_0, newx = test[,-c(1,2)], newy = test[,1], s = "lambda.min")

## test data R^2 = 0.2804
testpred = predict(depfits$alpha_0, s = "lambda.min", newx = test[,-c(1,2)])
testlm = lm(test[,1] ~ testpred)
summary(testlm)

## bootstrap 95% CI for coefficients ----
alpha = 0
lambda = depfits$alpha_0$lambda.min

boot_glmnet <- function(x, indices, alpha, lambda, pf){
  
  xdat = x[indices,-c(1,2)]
  yvar = x[indices,1]
  
  fit = glmnet::glmnet(x = xdat, y = yvar, lambda = lambda, alpha = alpha, 
               penalty.factor = pf)
  
  output = coef(fit) |> as.matrix()

  return(output)
}


boot = boot(data = train, statistic = boot_glmnet, R = 3,
            alpha = alpha, lambda = lambda, pf = pf, stype = "i") |> 
  futurize(seed = TRUE)
