## PACKAGES
library(tidyverse)
library(glmnet)

set.seed(2024)

#train = readRDS("/home/s1659680/ABCD/data/y2_traindat.rds")
train = readRDS("DATA/y2_traindat.rds") |> na.omit() #4611 individuals
test = readRDS("DATA/y2_testdat.rds") |> na.omit() #1166

## scale and centre numeric variables
numcols = names(train)[lapply(train, is.numeric)==T]
train = train |> mutate_at(numcols, ~ c(scale(.)) )
test = test |> mutate_at(numcols, ~ c(scale(.)) )

## remove columns that are not used
x = train |> select(-src_subject_id, -race_ethnicity, -cbcl_internalising, -cbcl_dsm5_depress) |> data.matrix()

x_interact = x[,-grep("gender", colnames(x))] * as.numeric(train$gender)
colnames(x_interact) = paste0(colnames(x_interact), "_gender")

preds = cbind(x, x_interact)
dep = train$cbcl_dsm5_depress

depfit = cv.glmnet(x = preds, y = dep, nfolds = 10)
print(depfit)

y2_coefs = coef(depfit, s = "lambda.1se") |> as.matrix()

write.csv(y2_coefs, "OUTPUT/COMPLETE/y2_dep_coefs.csv")
saveRDS(depfit, "OUTPUT/COMPLETE/y2_dep_fit.rds")

#### test sample ####
test_x = test |> select(-src_subject_id, -race_ethnicity, -cbcl_internalising, -cbcl_dsm5_depress) |> data.matrix()

test_x_interact = test_x[,-grep("gender", colnames(test_x))] * as.numeric(test$gender)
names(test_x_interact) = paste0(names(test_x_interact), "_gender")

test_preds = cbind(test_x, test_x_interact)

print(depfit)

##### trying fitstats in the same file ##########

### training data 

datlist.train = datlist
rm(datlist)
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
mult = y2_coefs[2:length(y2_coefs)]
intercept = y2_coefs[1]


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

load("/home/s1659680/ABCD/data/y2_testdat_imp/y2_testdat_imp__DATALIST.Rdata")
load("/home/s1659680/ABCD/data/y2_testdat_imp/y2_testdat_imp.Rdata")

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
mult = y2_coefs[2:length(y2_coefs)]
intercept = y2_coefs[1]


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

write.csv(result.stats, "/home/s1659680/ABCD/output/y2_dep_results.csv")