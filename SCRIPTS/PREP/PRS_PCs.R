library(tidyverse)
pcs = read.delim("/exports/igmm/eddie/GenScotDepression/users/poppy/abcd/multian/git-ancestries/data/abcd.randomforest.ancestries.tsv", header=TRUE)
pcs = pcs |> select("IID", "PC1_AVG", "PC2_AVG", "PC3_AVG", "PC4_AVG", "PC5_AVG")

prs = readRDS("/exports/igmm/datastore/GenScotDepression/users/eileen/ABCD/ABCD_Environmental_Risk/ABCDv5.1/DATA/mdd25_prs.rds")

## merge in PCs

for (anc in 1:length(prs)) {
  df = prs[[anc]]
  newdf = merge(df, pcs, by = "IID", all.y = FALSE)
  prs[[anc]] = newdf
}

glimpse(prs)

saveRDS(prs, "/exports/igmm/datastore/GenScotDepression/users/eileen/ABCD/ABCD_Environmental_Risk/ABCDv5.1/DATA/mdd25_prs.rds")
