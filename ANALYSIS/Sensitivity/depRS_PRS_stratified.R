## depRS + PRS analyses stratified by ancestry ##

here::i_am("ANALYSIS/Sensitivity/depRS_PRS_stratified.R")

# Packages ----
library(tidyverse)
library(performance)
library(pROC)
library(lme4)
library(broom.mixed)

# Read in data and scale depRS and PCs
dat = readRDS("DATA/depRS_PRS.rds")
AFR = dat |> filter(ANC=="AFR") 
AMR = dat |> filter(ANC=="AMR") 
EAS = dat |> filter(ANC=="EAS")
EUR = dat |> filter(ANC=="EUR_div") 

dfs.y = list("AFR" = AFR, "AMR" = AMR, "EAS" = EAS, "EUR" = EUR) |> 
  map(\(df) filter(df, !is.na(mdd.y) & incident.y!="exclude") |> droplevels() |> 
        mutate(depRS = scale(depRS), across(PC1_AVG:PC5_AVG, scale)))
dfs.y |> map(\(df) summary(df[,c("site_id_l", "mdd.y")]))

dfs.p = list("AFR" = AFR, "AMR" = AMR, "EAS" = EAS, "EUR" = EUR) |> 
  map(\(df) filter(df, !is.na(mdd.p) & incident.p!="exclude") |> droplevels() |> 
        mutate(depRS = scale(depRS), across(PC1_AVG:PC5_AVG, scale)))
dfs.p |> map(\(df) summary(df[,c("site_id_l", "mdd.p")]))

ctrl = glmerControl(optimizer = "bobyqa")

rm(AFR, AMR, EAS, EUR)

# Model RHS
rhs = list(
  "m1depRS" = "~ depRS + (1 | site_id_l)",
  "m1PRS" = "~ PRS.z + PC1_AVG + PC2_AVG + PC3_AVG + PC4_AVG + PC5_AVG + (1 | site_id_l)",
  "m2" = " ~ depRS + PRS.z + PC1_AVG + PC2_AVG + PC3_AVG + PC4_AVG + PC5_AVG + (1 | site_id_l)",
  "m3" = "~ depRS*PRS.z + PC1_AVG + PC2_AVG + PC3_AVG + PC4_AVG + PC5_AVG + (1 | site_id_l)"
)

# function to loop over RHS inputs using dfs passed from map()
loop_fun <- function(df, rhs, out){
  for (r in 1:length(rhs)) {
    
    # create output df on first iteration
    if(r==1){res=data.frame()}
    
    # fit model
    anc = as.character(unique(df$ANC))
    mod = names(rhs[r])
    form = as.formula(paste0(out, rhs[[r]]))
    fit = glmer(form, data = df, family = binomial(link = "logit"),
              control = ctrl)
  
    # fit measures
    r2 = r2_tjur(fit) |> as.numeric()
    rmse = rmse(fit)
  
    # auc
    x = predict(fit, df)
    y = df[,out]
    ci = roc(response = y, predictor = x, auc = TRUE, ci = TRUE)$ci
    auc = ci[2]
    lower = ci[1]
    upper = ci[3]
    
    # add to output df
    row = data.frame("anc" = anc, "mod" = mod, "r2" = r2, "rmse" = rmse, "auc" = auc, 
                   "auc.lower" = lower, "auc.upper" = upper)
    
    res = rbind(res, row)
  }
  # round to 3dp
  res = res |> mutate(across(where(is.numeric), ~ round(.x, digits = 3)))
  
  return(res)
}

# function to run model comparisons using dfs passed from map()
map_lrt <- function(df, rhs, out){
    fitlist = map(rhs, \(r)
      glmer(formula = as.formula(paste0(out, r)), data = df, 
            family = binomial(link = "logit"), control = ctrl))
    
    depRS_perf = test_performance(fitlist[-2])
    PRS_perf = test_performance(fitlist[-1])
    
    res = rbind(depRS_perf, PRS_perf)
    anc = rep(df$ANC[1], times=nrow(res))
    out = cbind(anc, res)
    return(out)
    }

# Loop over ancestries and RHS
lrt.y = dfs.y[-3] |> map(\(df) map_lrt(df = df, rhs = rhs, out = "mdd.y")) |> 
  reduce(rbind)
fit.y = dfs.y[-3] |> map(\(df) loop_fun(df = df, rhs = rhs, out = "mdd.y")) |> 
  reduce(rbind)
lrt.p = dfs.p[-3] |> map(\(df) map_lrt(df = df, rhs = rhs, out = "mdd.p")) |> 
  reduce(rbind)
fit.p = dfs.p[-3] |> map(\(df) loop_fun(df = df, rhs = rhs, out = "mdd.p")) |> 
  reduce(rbind)

results = list(lrt.y, fit.y, lrt.p, fit.p) |> set_names(c("lrt.y", "fit.y", "lrt.p", "fit.p"))

openxlsx::write.xlsx(results, "ANALYSIS/OUT/ancestry_stratified.xlsx")
