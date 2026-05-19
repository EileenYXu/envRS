#################################################
## MERGE PRS AND ANCESTRY PCS, CALCULATE depRS ##
#################################################

renv::load()
here::i_am("PREP/depRS_PRS_PCs.R")

library(tidyverse)

## Read in PRS for each ancestry ----

## EUR
EUR_div = read.table("PRS/pgc_mdd24_div_ukbEUR_HM3.profile", header = TRUE)
EUR_eur = read.table("PRS/pgc_mdd24_eur_ukbEUR_HM3.profile", header = TRUE)

## AFR
AFR = read.table("PRS/pgc_mdd24_div_ukbAFR_HM3.profile", header = TRUE)

## EAS
EAS = read.table("PRS/pgc_mdd24_div_ukbEAS_HM3.profile", header = TRUE)

## AMR
AMR = read.table("PRS/pgc_mdd24_div_ukbEUR_HM3_AMRsample.profile", header = TRUE)

PRS = list("EUR_div" = EUR_div, "EUR_eur" = EUR_eur, "AFR" = AFR, "EAS" = EAS, "AMR" = AMR)
PRS = lapply(PRS, \(x) select(x, IID, SCORESUM))


## Read in PCs ----

PCS = read.delim("/exports/igmm/eddie/GenScotDepression/users/poppy/abcd/multian/git-ancestries/data/abcd.randomforest.ancestries.tsv", header = TRUE)
PCS = PCS |> select("IID", "PC1_AVG", "PC2_AVG", "PC3_AVG", "PC4_AVG", "PC5_AVG")

## merge in PCs

for (anc in 1:length(PRS)) {
  df = PRS[[anc]]
  newdf = merge(df, PCS, by = "IID", all.y = FALSE)
  PRS[[anc]] = newdf
}

## standardise PRS within each ancestry grouping ----
df.prs = imap(PRS, \(x, idx) mutate(x, ANC = idx)) |> reduce(rbind)
z.prs = map(PRS, \(x) scale(x$SCORESUM)) |> reduce(rbind)
df.prs$PRS.z = z.prs[,1]          
df.prs$ANC = as.factor(df.prs$ANC)

saveRDS(df.prs, "DATA/mdd25_prs.rds")

## Calculate depRS in unrelated sample ----

df.prs = readRDS("DATA/mdd25_prs.rds")

# Read in coefficients, remove intercept and non-significant coefficients
coefs = read.csv("ANALYSIS/OUT/depRS_main_Y2.csv")
coefs = coefs[-1,] |> select(Predictor, Estimate, Sig) |> filter(Sig=="Y")
coefs$Predictor

# Keep subject ID, site ID, gender, birthsex and CBCL depression
preds = c("src_subject_id", "site_id_l", "gender", "birthsex", 
          "cbcl_dsm5_depress", coefs$Predictor)

## Prep sample
dat = readRDS("DATA/predictors_unrelatedIDs.rds") |> 
  filter(eventname=="baseline") |> 
  select(all_of(preds)) |> droplevels() 

complete = dat[complete.cases(dat[,coefs$Predictor]),] |> filter(gender != "GNC")
nrow(complete) #8092 complete cases
rm(dat)

# binary predictors to be dummy coded as 0/1 
binpreds = c("tobacco_puff", "weightcontrol_ksads", "death_loved_one", 
             "witness_dv", "s_abuse", "emot_abuse", "serious_accident", 
             "needed_food")

# numeric predictors to be centred and scaled
numpreds = c("sleep_hrs", "bmi", "income", "comm_safety", 
             "days_acti", "fam_conflict", "p_monitoring", 
             "p_acceptance", "p_depression", "interview_age")

p = complete |> mutate(
  across(all_of(binpreds), ~ as.numeric(.x) - 1),
  across(all_of(numpreds),  ~ scale(as.numeric(.x)))) |> 
  select(all_of(coefs$Predictor)) |> data.matrix()

depRS = coefs$Estimate %*% t(p) |> t()

complete$depRS = depRS[,1]
complete = complete |> 
  select(src_subject_id, site_id_l, gender, birthsex, interview_age,
         depRS, cbcl_dsm5_depress)

rm(depRS, p, coefs, binpreds, numpreds, preds)

## merge in PRS ----
div.prs = df.prs |> filter(ANC!="EUR_eur")
sens.prs = df.prs |> filter(ANC!="EUR_div") |> select(IID, PRS.z) |> setNames(c("IID", "PRS.sens"))

depRS.PRS = merge(complete, div.prs, by.x = "src_subject_id", by.y = "IID")
depRS.PRS = merge(depRS.PRS, sens.prs, by.x = "src_subject_id", by.y = "IID")
nrow(depRS.PRS) #7283 with depRS + PRS

rm(df.prs, div.prs, sens.prs, complete)

## merge in lifetime MDD ----
mdd.y = readRDS("DATA/ksads_y.rds")
mdd.p = readRDS("DATA/ksads_p.rds")

mdd.y = mdd.y |> select(src_subject_id, mdd_lifetime, mdd_incident) |> 
  setNames(c("id", "mdd.y", "incident.y"))
mdd.p = mdd.p |> select(src_subject_id, mdd_lifetime, mdd_incident) |> 
  setNames(c("id", "mdd.p", "incident.p"))

mdd.all = merge(mdd.p, mdd.y, by = "id", all = TRUE)
depRS.PRS = merge(depRS.PRS, mdd.all, by.x = "src_subject_id", by.y = "id", all.x = TRUE)

sum(!is.na(depRS.PRS$mdd.y)) #6736 with self-reported lifetime MDD data
sum(!is.na(depRS.PRS$mdd.p)) #6660 with parent-reported lifetime MDD data

summary(depRS.PRS[,17:20])
any(duplicated(depRS.PRS))

saveRDS(depRS.PRS, "DATA/depRS_PRS.rds")
