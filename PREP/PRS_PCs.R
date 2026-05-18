###################################
## MERGE IN PRS AND ANCESTRY PCS ##
###################################

renv::load()
here::i_am("PREP/PRS_PCs")

library(tidyverse)

pcs = read.delim("/exports/igmm/eddie/GenScotDepression/users/poppy/abcd/multian/git-ancestries/data/abcd.randomforest.ancestries.tsv", header=TRUE)
pcs = pcs |> select("IID", "PC1_AVG", "PC2_AVG", "PC3_AVG", "PC4_AVG", "PC5_AVG")

prs = readRDS("/DATA/mdd25_prs.rds")

## merge in PCs

for (anc in 1:length(prs)) {
  df = prs[[anc]]
  newdf = merge(df, pcs, by = "IID", all.y = FALSE)
  prs[[anc]] = newdf
}

df.prs = imap(prs, \(x, idx) mutate(x, ANC = idx)) |> reduce(rbind)
z.prs = map(prs, \(x) scale(x$SCORESUM)) |> reduce(rbind)

df.prs$PRS.z = z.prs[,1]          

saveRDS(df.prs, "/DATA/mdd25_prs.rds")
