source("./SCRIPTS/summarystats.R")
library(tidyverse)

############ Read in all data ############

# whole ABCD baseline sample
baseline = readRDS("G://users/eileen/ABCD/ABCD_Environmental_Risk/ABCDv5.1/DATA/predictors_allIDs.rds") |> 
  filter(eventname=="baseline_year_1_arm_1") |> 
  mutate(age_base = interview_age/12, income = as.factor(income)) |> 
  select(src_subject_id, eventname, age_base, gender_id, birthsex, race_ethnicity, income, parent_ed)

y2_age = readRDS("G://users/eileen/ABCD/ABCD_Environmental_Risk/ABCDv5.1/DATA/predictors_allIDs.rds") |> 
  filter(eventname=="2_year_follow_up_y_arm_1") |> 
  mutate(age_y2 = interview_age/12) |> 
  select(src_subject_id, age_y2)

fullsamp = merge(baseline, y2_age, by = "src_subject_id", all.x = T)

depRS_ids = readRDS("G://users/eileen/ABCD/ABCD_Environmental_Risk/ABCDv5.1/DATA/EN_ids.rds")

#in long depRS
longsamp = readRDS("G://users/eileen/ABCD/ABCD_Environmental_Risk/ABCDv5.1/DATA/depRS_long.rds") |> select(src_subject_id)

#in cross-sectional depRS
crosssamp = readRDS("G://users/eileen/ABCD/ABCD_Environmental_Risk/ABCDv5.1/DATA/depRS_baseline.rds") |> select(src_subject_id)

#in depRS/PRS

mdd2025 = readRDS("G://users/eileen/ABCD/ABCD_Environmental_Risk/ABCDv5.1/DATA/mdd25_prs.rds")

for (anc in 1:length(mdd2025)) {
  ancestry = names(mdd2025)[anc]
  val = rep(ancestry, times = nrow(mdd2025[[anc]]))
  mdd2025[[anc]]$ancestry = as.factor(val)
}

PRS = bind_rows(mdd2025$afr, mdd2025$eas, mdd2025$amr_diverse, mdd2025$eur_diverse) |> select(IID, ancestry)

fullsamp = merge(fullsamp, PRS, by.x = "src_subject_id", by.y = "IID", all.x = T)

scores = PRS |> filter(IID %in% longsamp$src_subject_id)

### youth #############
mddy = readRDS("G://users/eileen/ABCD/ABCD_Environmental_Risk/ABCDv5.1/DATA/ksads_y.rds")
dat.y = merge(scores, mddy$y2[,c("src_subject_id", "mdd_lifetime")], by.x = "IID",by.y = "src_subject_id")

mddy = mddy$y2 |> select(src_subject_id, mdd_lifetime) 
names(mddy)[2] = "mdd_lifetime_y"

##### parent #############
mddp = readRDS("G://users/eileen/ABCD/ABCD_Environmental_Risk/ABCDv5.1/DATA/ksads_p.rds")
dat.p = merge(scores, mddp$y2, by.x = "IID",by.y = "src_subject_id")

mddp = mddp$y2 |> select(src_subject_id, mdd_lifetime) 
names(mddp)[2] = "mdd_lifetime_p"

##### included and excluded columns ##########

fullsamp = merge(fullsamp, mddp, by = "src_subject_id", all.x = T)
fullsamp = merge(fullsamp, mddy, by = "src_subject_id", all.x = T)

fullsamp = fullsamp |> mutate(
  gender_id = fct_collapse(gender_id, GNC = c("GNC", "Diff")) |>  fct_relevel(c("M", "F", "Mtrans", "Ftrans", "GNC")),
  birthsex = fct_relevel(birthsex, c("M", "F", "Intersex")),
  race_ethnicity = fct_relevel(race_ethnicity, c("Asian", "Black", "Hispanic", "White", "Other")),
  parent_ed = fct_relevel(parent_ed, c("less_HS", "HS/GED", "Some_College", "Bachelor", "Postgraduate")),
  ancestry = fct_relevel(ancestry, c("afr", "amr_diverse", "eas", "eur_diverse")),
  in_depRS_long = ifelse(src_subject_id %in% longsamp$src_subject_id, 1, 0),
  in_depRS_cross = ifelse(src_subject_id %in% crosssamp$src_subject_id, 1, 0),
  in_PRS_y = ifelse(src_subject_id %in% dat.y$IID, 1, 0),
  in_PRS_p = ifelse(src_subject_id %in% dat.p$IID, 1, 0),
  in_train_long = ifelse(src_subject_id %in% depRS_ids$long_train, 1, 0),
  in_test_long = ifelse(src_subject_id %in% depRS_ids$long_test, 1, 0),
  in_train_cross = ifelse(src_subject_id %in% depRS_ids$cross_train, 1, 0),
  in_test_cross = ifelse(src_subject_id %in% depRS_ids$cross_test, 1, 0)
)

fullsamp = fullsamp |> relocate(age_y2, .after = age_base)

samps = c("in_depRS_long", "in_depRS_cross", "in_PRS_y", "in_PRS_p")

desc_env = c("age_base", "age_y2", "gender_id", "birthsex", "race_ethnicity", "income", "parent_ed", "ancestry", "mdd_lifetime_p", "mdd_lifetime_y")

descriptives = list()

for (s in samps) {
  indat = fullsamp |> filter(!!sym(s) == 1) |> get_sum_stats(vars = desc_env)
  names(indat)[2:3] = c("N_inc", "Sumstat_inc")
  exdat = fullsamp |> filter(!!sym(s) == 0) |> get_sum_stats(vars = desc_env)
  names(exdat)[2:3] = c("N_exc", "Sumstat_exc")
  descriptives[[str_remove(s, "in_")]] = merge(indat, exdat, by = "Var", sort = F)
}

#openxlsx::write.xlsx(descriptives, "C://Users/eilee/Desktop/PhD/Year 3/Drafts/envRS_paper/Descriptives/ALL_EXCLUDED_DESC.xlsx")


depRS_samps = c("in_train_long", "in_test_long", "in_train_cross", "in_test_cross")

depRS_desc = data.frame("Var" = descriptives$depRS_long$Var)

for (s in depRS_samps) {
  tmpdat = fullsamp |> filter(!!sym(s) == 1) |> get_sum_stats(vars = desc_env)
  names(tmpdat)[3] = paste0(s, "_", tmpdat[1,2])
  depRS_desc = merge(depRS_desc, tmpdat[,-2], by = "Var", sort = F)
}

write.csv(depRS_desc, "C://Users/eilee/Desktop/PhD/Year 3/Drafts/envRS_paper/Descriptives/train_test_samps.csv")

########### by lifetime MDD status ##################
mdds = c(case = "1", ctrl = "0", miss = NA_character_)

##### parent-reported analyses #######
sumvars = c("age_base", "age_y2", "gender_id", "birthsex", "race_ethnicity", "income", "parent_ed", "ancestry", "mdd_lifetime_y")
descs = list()
for (m in 1:3) { # loop through possible MDD statuses
  inc = fullsamp |> filter(in_PRS_p == 1 & mdd_lifetime_p == as.character(mdds[m]))
  exc = fullsamp |> filter(in_PRS_p == 0 & mdd_lifetime_p == as.character(mdds[m]))
      if (nrow(inc) != 0) { # don't try and summarize an empty dataset
        descs[[paste0("in_p_",names(mdds[m]))]] = get_sum_stats(inc, sumvars)
      }
      
      if (nrow(exc) != 0) {
        descs[[paste0("exc_p_",names(mdds[m]))]] = get_sum_stats(exc, sumvars)
      }
    }

###### youth-reported analyses ###########
sumvars = c("age_base", "age_y2", "gender_id", "birthsex", "race_ethnicity", "income", "parent_ed", "ancestry", "mdd_lifetime_p")

for (m in 1:3) { # loop through possible MDD statuses
  inc = fullsamp |> filter(in_PRS_y == 1 & mdd_lifetime_y == as.character(mdds[m]))
  exc = fullsamp |> filter(in_PRS_y == 0 & mdd_lifetime_y == as.character(mdds[m]))
  if (nrow(inc) != 0) { # don't try and summarize an empty dataset
    descs[[paste0("in_y_",names(mdds[m]))]] = get_sum_stats(inc, sumvars)
  }
  
  if (nrow(exc) != 0) {
    descs[[paste0("exc_y_",names(mdds[m]))]] = get_sum_stats(exc, sumvars)
  }
}

openxlsx::write.xlsx(descs, "C://Users/eilee/Desktop/PhD/Year 3/Drafts/envRS_paper/Descriptives/PRS_Case_Ctrls.xlsx")
