###################################
## MERGE IN PRS AND ANCESTRY PCS ##
###################################

renv::load()
here::i_am("PREP/PRS_PCs.R")

library(tidyverse)

## Read in PRS for each ancestry

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

PCS = read.delim("/exports/igmm/eddie/GenScotDepression/users/poppy/abcd/multian/git-ancestries/data/abcd.randomforest.ancestries.tsv", header = TRUE)
PCS = PCS |> select("IID", "PC1_AVG", "PC2_AVG", "PC3_AVG", "PC4_AVG", "PC5_AVG")

## merge in PCs

for (anc in 1:length(PRS)) {
  df = PRS[[anc]]
  newdf = merge(df, PCS, by = "IID", all.y = FALSE)
  PRS[[anc]] = newdf
}

## standardise PRS within each ancestry grouping
df.prs = imap(PRS, \(x, idx) mutate(x, ANC = idx)) |> reduce(rbind)
z.prs = map(PRS, \(x) scale(x$SCORESUM)) |> reduce(rbind)
df.prs$PRS.z = z.prs[,1]          
df.prs$ANC = as.factor(df.prs$ANC)

saveRDS(df.prs, "DATA/mdd25_prs.rds")
