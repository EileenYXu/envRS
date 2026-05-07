######################################################
## Impute missing baseline predictors and CBCL data ##
######################################################

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

# remove predictors with >20% missing data, illicit drug use and ids
base = base |> select_if(~ sum(is.na(.))<0.2*nrow(base)) |>
  select(-c(eventname, illicit, rel_family_id, rel_birth_id, 
            interview_date, visit_type)) |> droplevels()

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
# comm_safety = 0.094294770

## impute using mice ----
svg("PREP/mdpattern.svg", width = 30, height = 30)
md.pattern(base, rotate.names = TRUE) |> t()
dev.off()

# make predictor matrix and method vector
pred = make.predictorMatrix(base)
meth = make.method(base)

# set site_id_l as the cluster
pred[,"src_subject_id"] = 0
#pred[,"site_id_l"]= -2 # -2 denotes cluster - figure out do I need to do this??
pred[,"gender_id"] = 0 # multicollinearitly between gender and birthsex
pred[,"birthsex"] = 0 # keeping gender only

# impute 50 datasets ----
base_imp = mice(base, seed = 2404, predictorMatrix = pred, method = meth, 
                m=50, print = FALSE)

save(base_imp, file = "DATA/baseline_imputed.RData")
