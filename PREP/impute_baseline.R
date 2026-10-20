######################################################
## Impute missing baseline predictors and CBCL data ##
######################################################

renv::load()

## packages ----
library(tidyverse)
library(mice)
library(here)
library(miceadds)
here()

## read in data ----
dat = readRDS("DATA/predictors_unrelatedIDs.rds")

## make wide CBCL ----
cbcl_wide = dat |> select(src_subject_id, eventname, cbcl_internalising, cbcl_dsm5_depress) |> pivot_wider(names_from = "eventname", values_from = c("cbcl_dsm5_depress", "cbcl_internalising"))

## tidy baseline data ---- 
base = dat |> filter(eventname=="baseline") |> 
  select_if(~ !all(is.na(.))) |> droplevels() 
bvars = names(base)

# remove predictors with >20% missing data, illicit drug use and unused 
# variables - interview details, gender_id 
base = base |> select_if(~ sum(is.na(.))<0.2*nrow(base)) |>
  select(-c(eventname, illicit, rel_family_id, rel_birth_id, 
            interview_date, visit_type, gender_id)) |> droplevels()

bvars[bvars %in% names(base)==F] # checking which were removed

# merge baseline predictors with y2 CBCL
base = merge(base, cbcl_wide[,c(1,2,4)], by = "src_subject_id", all.x = TRUE)
summary(base)

## check ICCs for study site ----
numdat = base |> select(site_id_l, where(is.numeric))
out = c()
for (var in names(numdat)[-1]) {
  icc = ICC::ICCbare(x = site_id_l, y=var, data = numdat)
  out = c(out, icc)
}

data.frame(vars = names(numdat)[-1], ICC = out) |> sort_by(~ desc(ICC)) 
# area_depriv = 0.434648072
# income = 0.130426998

## heatmap of correlations ----
svg("PLOTS/corplot.svg", height = 8, width = 8)
cor(numdat[,-1], use = "pairwise.complete.obs") |> 
  reshape2::melt() |> 
  ggplot(aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(high = "red", low = "blue", mid = "white") + labs(x = NULL, y = NULL) + theme(axis.text.x = element_text(angle = 45))
dev.off()

## begin imputation using mice ----
svg("PLOTS/mdpattern.svg", height = 30)
md.pattern(base, rotate.names = TRUE) |> t()
dev.off()

# make predictor matrix and method vector
pred = make.predictorMatrix(base)
meth = make.method(base)

# remove variables which are not used for imputation
pred[,"src_subject_id"] = 0

# multilevel imputation for area_depriv ----
pred["area_depriv",] = 0
meth["area_depriv"] = "2l.pmm"

# cluster by site, use cluster means of income as contextual predictor/covariate, plus parent_ed and race_ethnicity as simple covariates
pred["area_depriv", c("site_id_l", "income", "parent_ed", "race_ethnicity")] = c(-2, 3, 1, 1)

# cluster must be integer for imputation
base$site_id_l = as.integer(base$site_id_l)

# impute 50 datasets ----
base_imp = mice(base, seed = 2404, predictorMatrix = pred, method = meth, m=50, maxit = 5)

# check for convergence ----
svg("PLOTS/base_imp_tracelines.svg", height = 30)
plot(base_imp, layout = c(2, 30))
dev.off()

# add more iterations to check
imp2 = mice.mids(base_imp, maxit = 25)
svg("PLOTS/base_imp2_tracelines.svg", height = 30)
plot(imp2, layout = c(2, 30))
dev.off()

save(base_imp, file = "DATA/baseline_imputed.RData")
