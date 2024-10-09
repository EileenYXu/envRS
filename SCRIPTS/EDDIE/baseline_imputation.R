### SCRIPT FOR MULTIPLE IMPUTATION OF ABCD DATA ###

## PACKAGES
library(tidyverse)
library(mice)
library(miceadds)

set.seed(2024) # for reproducibility

## read in data
traindat = readRDS("/home/s1659680/ABCD/data/baseline_traindat.rds")
testdat = readRDS("/home/s1659680/ABCD/data/baseline_testdat.rds")

# make vector of imputation methods, based on whether the data are continuous, binary or categorical

traindat = traindat[,-grep("gender_id", names(traindat))]
testdat = testdat[,-grep("gender_id", names(testdat))]

setwd("/home/s1659680/ABCD/data")

### impute test data
testdat = droplevels(testdat)

testpredMat = make.predictorMatrix(testdat)

testdat_imputed <- mice(testdat, m=5, predictorMatrix=testpredMat, 
                    seed=2024, printFlag=F, maxit=50)

pdf("/home/s1659680/ABCD/output/testdat_miceplots.pdf")
plot(testdat_imputed)
dev.off()

write.mice.imputation(mi.res=testdat_imputed, name="baseline_testdat_imp",
                      include.varnames=T, dattype="csv", mids2spss = F, long = T)


### impute training data

trainpredMat = make.predictorMatrix(traindat)

traindat_imputed <- mice(traindat, m=5, predictorMatrix=trainpredMat, 
                    seed=2024, printFlag=F, maxit=50)

pdf("/home/s1659680/ABCD/output/traindat_miceplots.pdf")
plot(traindat_imputed)
dev.off()

write.mice.imputation(mi.res=traindat_imputed, name="baseline_traindat_imp",
                      include.varnames=T, dattype="csv", mids2spss = F, long = T)
