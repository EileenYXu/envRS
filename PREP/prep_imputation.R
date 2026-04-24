#### Multiple imputation of missing data ####

#packages
library(tidyverse)
library(mice)
library(here)

#read in all data
dat = readRDS("/DATA/predictors_unrelatedIDs.rds")

