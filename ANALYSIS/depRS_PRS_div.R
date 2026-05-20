######################################################
## Predicting binary MDD status using depRS and PRS ##
######################################################

# This script predicts (a) self-reported and (b) parent-reported lifetime MDD at Year 2 from depRS and MDD PRS, excluding individuals with MDD history at baseline.

renv::load()
here::i_am("ANALYSIS/depRS_PRS_div.R")

# Packages ----
library(tidyverse)
library(performance)
library(pROC)
library(lme4)
library(broom.mixed)

# Read in data and scale depRS and PCs. PRS.z already scaled across each ancestry
dat = readRDS("DATA/depRS_PRS.rds") |> mutate(depRS = scale(depRS),
         across(PC1_AVG:PC5_AVG, scale))

ctrl = glmerControl(optimizer = "bobyqa")

# (a) self-reported lifetime MDD (N = 6551) ----
dat.y = dat |> filter(!is.na(mdd.y) & incident.y!="exclude") |> droplevels()

m1depRS.y = glmer(mdd.y ~ depRS + (1 | site_id_l), dat.y, 
                  family = binomial(link = "logit"), control = ctrl)

m1PRS.y = glmer(mdd.y ~ PRS.z + PC1_AVG + PC2_AVG + PC3_AVG + PC4_AVG + PC5_AVG +
                (1 | site_id_l), dat.y, family = binomial(link = "logit"),
                control = ctrl)

m2.y = glmer(mdd.y ~ depRS + PRS.z + PC1_AVG + PC2_AVG + PC3_AVG + PC4_AVG + 
             PC5_AVG + (1 | site_id_l), dat.y, family = binomial(link = "logit"),
             control = ctrl)

m3.y = glmer(mdd.y ~ depRS*PRS.z + PC1_AVG + PC2_AVG + PC3_AVG + PC4_AVG + 
             PC5_AVG + (1 | site_id_l), dat.y, family = binomial(link = "logit"),
             control = ctrl)

## Extract results ----

## model comparisons 
depRS_perf = test_performance(m1depRS.y, m2.y, m3.y)
PRS_perf = test_performance(m1PRS.y, m2.y, m3.y)

y.mods = list("depRS" = m1depRS.y, "PRS" = m1PRS.y, "depRS+PRS" = m2.y,
          "depRS*PRS" = m3.y)

## fit measures
r2 = y.mods |> map_dbl(\(x) r2_tjur(x))
rmse = y.mods |> map_dbl(\(x) rmse(x))
fit = data.frame("r2" = r2, "rmse" = rmse)

dat.y = dat.y |> mutate(
  m1depRS = predict(m1depRS.y, dat.y),
  m1PRS = predict(m1PRS.y, dat.y),
  m2 = predict(m2.y, dat.y),
  m3 = predict(m3.y, dat.y)
) |> select(mdd.y, m1depRS, m1PRS, m2, m3, depRS, PRS.z)

preds = list("depRS" = dat.y$m1depRS, "PRS" = dat.y$m1PRS, 
             "depRS+PRS" = dat.y$m2, "depRS-PRS interaction" = dat.y$m3)

rocs.y = preds |> 
  map(\(x) roc(response = dat.y$mdd.y, predictor = x, auc = TRUE, ci = TRUE))

auc = rocs.y |> 
  map(\(x) data.frame("AUC" = x$ci[2], "Lower" = x$ci[1],
                      "Upper" = x$ci[3])) |> list_rbind(names_to = "Model")
fit = cbind(auc, fit)

res = list("fits" = fit, "depRS modcomp" = depRS_perf, "PRS modcomp" = PRS_perf)

## estimates
predictors = c("depRS", "PRS.z", "depRS:PRS.z")

ests = y.mods |> map(\(x) tidy(x, conf.int = TRUE, conf.level = 0.95, 
                               exponentiate = TRUE, effects = "fixed") |> 
                       filter(term %in% predictors))
res = append(res, ests)
names(res)[7] = "depRS-PRS interaction"

# save ROC curves
saveRDS(rocs.y, "DATA/main_rocs_y.rds")

# write results
openxlsx::write.xlsx(res, "ANALYSIS/OUT/depRS_PRS_y.xlsx")

rm(list = setdiff(ls(), c("dat", "ctrl")))

# (b) parent-reported lifetime MDD (N=6489) ----
dat.p = dat |> filter(!is.na(mdd.p) & incident.p!="exclude") |> droplevels()

m1depRS.p = glmer(mdd.p ~ depRS + (1 | site_id_l), dat.p, 
                  family = binomial(link = "logit"), control = ctrl)

m1PRS.p = glmer(mdd.p ~ PRS.z + PC1_AVG + PC2_AVG + PC3_AVG + PC4_AVG + PC5_AVG +
                  (1 | site_id_l), dat.p, family = binomial(link = "logit"),
                control = ctrl)

m2.p = glmer(mdd.p ~ depRS + PRS.z + PC1_AVG + PC2_AVG + PC3_AVG + PC4_AVG + 
               PC5_AVG + (1 | site_id_l), dat.p, family = binomial(link = "logit"),
             control = ctrl)

m3.p = glmer(mdd.p ~ depRS*PRS.z + PC1_AVG + PC2_AVG + PC3_AVG + PC4_AVG + 
               PC5_AVG + (1 | site_id_l), dat.p, family = binomial(link = "logit"),
             control = ctrl)

## Extract results ----

## model comparisons 
depRS_perf = test_performance(m1depRS.p, m2.p, m3.p)
PRS_perf = test_performance(m1PRS.p, m2.p, m3.p)

p.mods = list("depRS" = m1depRS.p, "PRS" = m1PRS.p, "depRS+PRS" = m2.p,
              "depRS*PRS" = m3.p)

## fit measures
r2 = p.mods |> map_dbl(\(x) r2_tjur(x))
rmse = p.mods |> map_dbl(\(x) rmse(x))
fit = data.frame("r2" = r2, "rmse" = rmse)

dat.p = dat.p |> mutate(
  m1depRS = predict(m1depRS.p, dat.p),
  m1PRS = predict(m1PRS.p, dat.p),
  m2 = predict(m2.p, dat.p),
  m3 = predict(m3.p, dat.p)
) |> select(mdd.p, m1depRS, m1PRS, m2, m3, depRS, PRS.z)

preds = list("depRS" = dat.p$m1depRS, "PRS" = dat.p$m1PRS, 
             "depRS+PRS" = dat.p$m2, "depRS-PRS interaction" = dat.p$m3)

rocs.p = preds |> 
  map(\(x) roc(response = dat.p$mdd.p, predictor = x, auc = TRUE, ci = TRUE))

auc = rocs.p |> 
  map(\(x) data.frame("AUC" = x$ci[2], "Lower" = x$ci[1],
                      "Upper" = x$ci[3])) |> list_rbind(names_to = "Model")
fit = cbind(auc, fit)

res = list("fits" = fit, "depRS modcomp" = depRS_perf, "PRS modcomp" = PRS_perf)

## estimates
predictors = c("depRS", "PRS.z", "depRS:PRS.z")

ests = p.mods |> map(\(x) tidy(x, conf.int = TRUE, conf.level = 0.95, 
                               exponentiate = TRUE, effects = "fixed") |> 
                       filter(term %in% predictors))
res = append(res, ests)
names(res)[7] = "depRS-PRS interaction"

# save ROC curves
saveRDS(rocs.p, "DATA/main_rocs_p.rds")

# write results
openxlsx::write.xlsx(res, "ANALYSIS/OUT/depRS_PRS_p.xlsx")
