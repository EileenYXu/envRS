## PACKAGES
library(tidyverse)
library(caret)
library(mice)
library(miselect)
library(miceadds)

## read in imputed training datasets
load("/home/s1659680/ABCD/data/baseline_traindat_imp/baseline_traindat_imp__DATALIST.Rdata")
load("/home/s1659680/ABCD/data/baseline_traindat_imp/baseline_traindat_imp.Rdata")


## scale and centre data

datlist.scaled = datlist

for (var in 1:ncol(datlist[[1]])) {
  for (i in 1:length(datlist)) {
    if (is.numeric(datlist[[i]][[var]])==T) {
      datlist.scaled[[i]][[var]] = scale(datlist[[i]][[var]], 
                                         center = T, scale = T)} 
    else {datlist.scaled[[i]][[var]] = datlist[[i]][[var]]}
  }
}

## miselect: internalising symptoms
set.seed(2024)

## remove columns that are not used
rem_cols = c("src_subject_id", "race_ethnicity", "cbcl_internalising", "cbcl_dsm5_depress")

x = list()
y = list()

for (i in 1:length(datlist.scaled)) {
	nointeract = data.matrix(datlist.scaled[[i]][,-which(colnames(datlist.scaled[[i]]) %in% rem_cols)])
	
	interactmat = nointeract[,-grep("gender", colnames(nointeract))] * nointeract[,grep("gender", colnames(nointeract))]	
	
	colnames(interactmat) = paste0(colnames(interactmat), "_gender")
	x[[i]] = cbind(nointeract, interactmat)
	y[[i]] = as.numeric(datlist.scaled[[i]]$cbcl_dsm5_depress)
}

# Calculate observational weights
weights  <- 1 - rowMeans(is.na(mi.res$data)) #weight by missing data proportion so that each individual is counted once
pf      <- rep(1, ncol(x[[1]])) #keep penalty factor the same for each var
adWeight <- rep(1, ncol(x[[1]])) #same as above
alpha = seq(0, 1, 0.01)

fit <- cv.saenet(x, y, pf = pf, adWeight = adWeight, weights = weights, alpha = alpha, nfolds = 10, maxit = 1000)

print(fit)
base_coefs = coef(fit)

write.csv(base_coefs, "/home/s1659680/ABCD/output/baseline_dep_coefs.csv")

saveRDS(fit, "/home/s1659680/ABCD/output/baseline_dep_fit.rds")

##### trying fitstats in the same file ##########

### training data 

datlist.train = datlist
rm(datlist)

datlist.scaled = datlist

for (var in 1:ncol(datlist[[1]])) {
  for (i in 1:length(datlist)) {
    if (is.numeric(datlist[[i]][[var]])==T) {
      datlist.scaled[[i]][[var]] = scale(datlist[[i]][[var]], 
                                         center = T, scale = T)} 
    else {datlist.scaled[[i]][[var]] = datlist[[i]][[var]]}
  }
}

outs = list()
preds = list()

# separate data into predictors and outcomes

for (i in 1:length(datlist.train)) {
	nointeract = data.matrix(datlist.train[[i]][,-which(colnames(datlist.train[[i]]) %in% rem_cols)])
	interactmat = nointeract[,-grep("gender", colnames(nointeract))] * nointeract[,grep("gender", colnames(nointeract))]	
	colnames(interactmat) = paste0(colnames(interactmat), "_gender")
	preds[[i]] = cbind(nointeract, interactmat)
	outs[[i]] = datlist.train[[i]][,grep("cbcl_", colnames(datlist.train[[i]]))]
}

## setup scores coef
mult = base_coefs[2:length(base_coefs)]
intercept = base_coefs[1]


# multiply to get scores
for (i in 1:length(preds)) {
  scores = c()
  for (n in 1:nrow(preds[[i]])) {
    subj = as.numeric(preds[[i]][n,])
    multiply = subj*mult
    RS = sum(multiply) + intercept
    scores = c(scores, RS)
  }
  outs[[i]]$depRS = scores
}

scoresout = datlist2mids(outs, progress=F)
int.mira = with(scoresout, lm(cbcl_internalising ~ depRS))
train.int.est = pool(int.mira) %>% summary() %>% mutate(dataset = rep("training"), outcome = rep("cbcl_internalising"))
train.int.r2 = pool.r.squared(int.mira)  %>% as.data.frame() %>% mutate(dataset = rep("training"), outcome = rep("cbcl_internalising"))
colnames(train.int.r2)[1] = "r2"

dep.mira = with(scoresout, lm(cbcl_dsm5_depress ~ depRS))
train.dep.est = pool(dep.mira) %>% summary() %>% mutate(dataset = rep("training"), outcome = rep("cbcl_dsm5_depress"))
train.dep.r2 = pool.r.squared(dep.mira) %>% as.data.frame() %>% mutate(dataset = rep("training"), outcome = rep("cbcl_dsm5_depress"))
colnames(train.dep.r2)[1] = "r2"

###################
#### test data ####
###################

load("/home/s1659680/ABCD/data/baseline_testdat_imp/baseline_testdat_imp__DATALIST.Rdata")
load("/home/s1659680/ABCD/data/baseline_testdat_imp/baseline_testdat_imp.Rdata")

datlist.test = datlist
rm(datlist)
outs = list()
preds = list()

# separate data into predictors and outcomes

for (i in 1:length(datlist.test)) {
	nointeract = data.matrix(datlist.test[[i]][,-which(colnames(datlist.test[[i]]) %in% rem_cols)])
	interactmat = nointeract[,-grep("gender", colnames(nointeract))] * nointeract[,grep("gender", colnames(nointeract))]	
	colnames(interactmat) = paste0(colnames(interactmat), "_gender")
	preds[[i]] = cbind(nointeract, interactmat)
	outs[[i]] = datlist.test[[i]][,grep("cbcl_", colnames(datlist.test[[i]]))]
}

## setup scores coef
mult = base_coefs[2:length(base_coefs)]
intercept = base_coefs[1]


# multiply to get scores
for (i in 1:length(preds)) {
  scores = c()
  for (n in 1:nrow(preds[[i]])) {
    subj = as.numeric(preds[[i]][n,])
    multiply = subj*mult
    RS = sum(multiply) + intercept
    scores = c(scores, RS)
  }
  outs[[i]]$depRS = scores
}

scoresout = datlist2mids(outs, progress=F)
int.mira = with(scoresout, lm(cbcl_internalising ~ depRS))
test.int.est = pool(int.mira) %>% summary() %>% mutate(dataset = rep("test"), outcome = rep("cbcl_internalising"))
test.int.r2 = pool.r.squared(int.mira)  %>% as.data.frame() %>% mutate(dataset = rep("test"), outcome = rep("cbcl_internalising"))
colnames(test.int.r2)[1] = "r2"

dep.mira = with(scoresout, lm(cbcl_dsm5_depress ~ depRS))
test.dep.est = pool(dep.mira) %>% summary() %>% mutate(dataset = rep("test"), outcome = rep("cbcl_dsm5_depress"))
test.dep.r2 = pool.r.squared(dep.mira) %>% as.data.frame() %>% mutate(dataset = rep("test"), outcome = rep("cbcl_dsm5_depress"))
colnames(test.dep.r2)[1] = "r2"

result.est = rbind(train.dep.est, test.dep.est, train.int.est, test.int.est)
result.est = result.est %>% filter(term=="depRS")
result.r2 = rbind(train.dep.r2, test.dep.r2, train.int.r2, test.int.r2)


result.stats = merge(result.est, result.r2, by = c("outcome", "dataset")) %>% mutate(dataset = factor(dataset, labels = c("training", "test")))

write.csv(result.stats, "/home/s1659680/ABCD/output/baseline_dep_results.csv")