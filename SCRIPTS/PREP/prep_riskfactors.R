library(tidyverse)

get95 = function(var){quantile(var, probs=0.95, na.rm=T)}

#### tlfb: alcohol, cannabis, polydrug use ####

tlfb = read.csv("G://data/abcd/release5.1/core/substance-use/su_y_tlfb.csv")

tlfb = tlfb |> select(src_subject_id, eventname, 
                       tlfb_cal_scr_alc_ud, #number of alcohol use days
                       tlfb_cal_scr_alc_max, #max units consumed in one sitting
                       su_tlfb_cal_scr_mj_days_yr, #number of cannabis use days
                       tlfb_cal_scr_num_events) #number substance use days

# code any days >365 as NA
is.na(tlfb$tlfb_cal_scr_alc_ud) = tlfb$tlfb_cal_scr_alc_ud > 365 
is.na(tlfb$tlfb_cal_scr_num_events) = tlfb$tlfb_cal_scr_num_events > 365
is.na(tlfb$su_tlfb_cal_scr_mj_days_yr) = tlfb$su_tlfb_cal_scr_mj_days_yr > 365

# code above 95 percentile as NA
is.na(tlfb$tlfb_cal_scr_alc_ud) = tlfb$tlfb_cal_scr_alc_ud > get95(tlfb$tlfb_cal_scr_alc_ud)
is.na(tlfb$tlfb_cal_scr_num_events) = tlfb$tlfb_cal_scr_num_events > get95(tlfb$tlfb_cal_scr_num_events)
is.na(tlfb$su_tlfb_cal_scr_mj_days_yr) = tlfb$su_tlfb_cal_scr_mj_days_yr > get95(tlfb$su_tlfb_cal_scr_mj_days_yr)
is.na(tlfb$tlfb_cal_scr_alc_max) = tlfb$tlfb_cal_scr_alc_max > get95(tlfb$tlfb_cal_scr_alc_max)

summary(tlfb)

# sui: illicit drug use, tobacco use ####
sui = read.csv("G://data/abcd/release5.1/core/substance-use/su_y_sui.csv")

sui = sui |> select(src_subject_id, eventname, tlfb_coc_use, tlfb_bsalts_use,
                     tlfb_meth_use, tlfb_mdma_use, tlfb_ghb_use, tlfb_opi_use,
                     tlfb_hall_use, tlfb_shrooms_use, tlfb_sniff_use, tlfb_tob_puff)

illictdrugs = c("tlfb_coc_use", "tlfb_bsalts_use",
                "tlfb_meth_use", "tlfb_mdma_use", "tlfb_ghb_use", "tlfb_opi_use",
                "tlfb_hall_use", "tlfb_shrooms_use", "tlfb_sniff_use")

sui = sui |> mutate(
  illicit = case_when(
    rowSums(na.rm = T, sui[,3:11])>0 ~ 1, 
    .default = 0) |> as.factor()
)

subs_use = merge(sui, tlfb, by = c("src_subject_id", "eventname"), all.x=T)
subs_use = subs_use |> select(src_subject_id, eventname, tlfb_cal_scr_alc_ud, tlfb_cal_scr_alc_max, su_tlfb_cal_scr_mj_days_yr, tlfb_cal_scr_num_events, illicit, tlfb_tob_puff) |> mutate(
 tlfb_tob_puff= as.factor(tlfb_tob_puff))

rm(sui, tlfb, illictdrugs)

##### ksads: symptom-weight control, ptsd scale traumatic events ####

ksads = read.csv("G://data/abcd/release5.1/core/mental-health/mh_p_ksads_ss.csv")
diet = ksads |> select(src_subject_id, eventname, ksads_13_72_p)
rm(ksads)

ptsd = read.csv("G://data/abcd/release5.1/core/mental-health/mh_p_ksads_ptsd.csv")
ptsd = ptsd |> select(src_subject_id, eventname, ksads_ptsd_raw_760_p, ksads_ptsd_raw_767_p, ksads_ptsd_raw_768_p, ksads_ptsd_raw_769_p, ksads_ptsd_raw_761_p, ksads_ptsd_raw_762_p, ksads_ptsd_raw_763_p, ksads_ptsd_raw_770_p, ksads_ptsd_raw_766_p, ksads_ptsd_raw_764_p, ksads_ptsd_raw_765_p, ksads_ptsd_raw_754_p, ksads_ptsd_raw_755_p)

# make trauma variables
ptsd = ptsd |> mutate(
  sa = case_when(rowSums(na.rm = T, ptsd[,4:6])>0 ~ 1, .default = 0) |> as.factor(),
  pa = case_when(rowSums(na.rm = T, ptsd[,7:9])>0 ~ 1, .default = 0) |> as.factor(),
  ea = case_when(rowSums(na.rm = T, ptsd[,12:13])>0 ~ 1, .default = 0) |> as.factor(),
  serious_accident = case_when(rowSums(na.rm = T, ptsd[,14:15])>0 ~ 1, .default = 0) |> as.factor()
)

ptsd = ptsd |> select(src_subject_id, eventname, ksads_ptsd_raw_760_p, ksads_ptsd_raw_770_p, 
                       ksads_ptsd_raw_766_p, sa, pa, ea, serious_accident)

ksads = merge(diet, ptsd, by = c("src_subject_id", "eventname"), all.x=T)
is.na(ksads$ksads_13_72_p)=ksads$ksads_13_72_p==888
ksads[,3:ncol(ksads)] = lapply(ksads[,3:ncol(ksads)], as.factor)
summary(ksads)
rm(diet, ptsd)

#### neglect scale - not available at baseline or y2 ####

#neglect = read.csv("G://data/abcd/release5.1/core/culture-environment/ce_y_mnbs.csv")
#neglect = neglect |> select(src_subject_id, eventname, mnbs_ss_mean_all)

#### bkfs ####
bkfs = read.csv("G://data/abcd/release5.1/core/physical-health/ph_p_bkfs.csv")
bkfs = bkfs |> select(src_subject_id, eventname, bkfs_fruit_ce, bkfs_vegnopot_ce, bkfs_dt_fibe)

#### erq coping #####
erq = read.csv("G://data/abcd/release5.1/core/mental-health/mh_y_erq.csv")
# prorated suppress_pr is broken because of course! should be prorate the sum of Qs 1,3,5
erq = erq |> mutate(across(where(is.numeric), ~na_if(.,777)))
erq = erq |> mutate(
  suppress_raw = rowSums(erq[,c(4, 5, 8)], na.rm = T),
  suppress_nm = rowSums(is.na(erq[,c(4, 5, 8)])==T),
  suppress_pr = suppress_raw/(3-suppress_nm)*3
)

erq = erq |> select(src_subject_id, eventname, suppress_pr)

#### sleep duration ####
sleep = read.csv("G://data/abcd/release5.1/core/physical-health/ph_p_sds.csv")
sleep = sleep |> select(src_subject_id, eventname, sleepdisturb1_p) |> mutate(
  sleep_hrs = factor(sleepdisturb1_p, labels = c("9+hrs", "8-9hrs", "7-8hrs","5-7hrs", "<5hrs"), ordered = T)
)
sleep$sleep_hrs = fct_rev(sleep$sleep_hrs)
sleep = sleep |> select(src_subject_id, eventname, sleep_hrs)

#### bmi ####
bmi = read.csv("G://users/eileen/ABCD/ABCD_Environmental_Risk/ABCDv5.1/DATA/BMI_modified_z.csv")
bmi = bmi |> filter(biv=="plausible") |> select(src_subject_id, eventname, bmi)

#### demogs ####
demog = read.csv("G://data/abcd/release5.1/core/abcd-general/abcd_p_demo.csv")
demog = demog |> select(src_subject_id, eventname, demo_gender_id_v2, 
                        demo_gender_id_v2_l, demo_sex_v2,
                        demo_fam_exp1_v2, demo_fam_exp1_v2_l,
                        demo_comb_income_v2, demo_comb_income_v2_l,
                        demo_prnt_ed_v2, demo_prnt_ed_v2_2yr_l,
                        demo_prnt_ed_v2_l, race_ethnicity) |> 
  mutate(across(where(is.numeric), ~na_if(.,777)),
         across(where(is.numeric), ~na_if(.,999)))

# sex and ethnicity data only collected at one timepoint so later timepoints need to be filled in first

demog = demog |> group_by(src_subject_id) |> fill(demo_sex_v2) |> 
  fill(race_ethnicity)
demog = ungroup(demog)

# gender, needed food, income and parent education were collected at multiple timepoints so need to be grouped into a single column

demog = demog |> mutate(
  needed_food = coalesce(demo_fam_exp1_v2, demo_fam_exp1_v2_l),
  income = coalesce(demo_comb_income_v2, demo_comb_income_v2_l),
  parent_ed = coalesce(demo_prnt_ed_v2, demo_prnt_ed_v2_2yr_l,
                              demo_prnt_ed_v2_l),
  gender_id = coalesce(demo_gender_id_v2, demo_gender_id_v2_l))

# recode factors with labels
demog = demog |> mutate(
  race_ethnicity = factor(race_ethnicity, 
                          labels = c("White", "Black","Hispanic", 
                                     "Asian", "Other")),
  birthsex = factor(demo_sex_v2,
                    labels =c("M", "F", "Intersex")),
  gender_id = factor(gender_id,
                     labels = c("M", "F", "Mtrans",
                                "Ftrans", "GNC", "Diff"))
)

# use case_when for parent education and for gender variable where cis and trans people merged as one
demog = demog |> mutate(
  parent_ed = case_when(
    is.na(parent_ed) ~ NA_character_,
    parent_ed < 13 ~ "less_HS",
    parent_ed == 13 | parent_ed == 14 ~ "HS/GED",
    parent_ed %in% c(15, 16, 17, 22, 23) ~ "Some_College",
    parent_ed == 18 ~ "Bachelor",
    parent_ed %in% c(19:21) ~ "Postgraduate") |> 
    as.factor(),
  
  gender = case_when(
    is.na(birthsex) & is.na(gender_id) ~ NA_character_,
    gender_id=="M" | gender_id=="Mtrans" | birthsex=="M" & is.na(gender_id) ~ "M",
    gender_id=="F" | gender_id=="Ftrans" | birthsex=="F" & is.na(gender_id) ~ "F",
    gender_id=="GNC" | gender_id=="Diff" ~ "GNC") |> 
    as.factor()
)


demog = demog |> select(src_subject_id, eventname, needed_food, income, parent_ed, birthsex, gender_id, gender, race_ethnicity) |> droplevels()
summary(demog)

#### deprivation #####
depriv = read.csv("G://data/abcd/release5.1/core/linked-external-data/led_l_adi.csv")

depriv = depriv |> select(src_subject_id, eventname, reshist_addr1_adi_perc)

#### community safety ####
commsafety = read.csv("G://data/abcd/release5.1/core/culture-environment/ce_p_nsc.csv")
commsafety = commsafety |> select(src_subject_id, eventname, nsc_p_ss_mean_3_items)
summary(commsafety)

#### discrimination ####
discrim = read.csv("G://data/abcd/release5.1/core/culture-environment/ce_y_dm.csv")
discrim = discrim |> select(src_subject_id, eventname, dim_y_ss_mean)
summary(discrim)

#### physical activity ####
activity = read.csv("G://data/abcd/release5.1/core/physical-health/ph_y_yrb.csv")
activity = activity |> select(src_subject_id, eventname, physical_activity1_y)
summary(activity)

#### screentime ####
screentime = read.csv("G://data/abcd/release5.1/core/novel-technologies/nt_y_st.csv")

screentime = screentime |> mutate(
  across(where(is.numeric), ~na_if(.,777)))

screentime$soc_media_add = rowSums(screentime[,grep("screentime_smqa", names(screentime))])
screentime$video_game_add = rowSums(screentime[,grep("screentime_vgaq", names(screentime))])

screentime = screentime |> select(src_subject_id, eventname, screentime_smq_soc_med_hr, screentime_smq_sm_min, soc_media_add, video_game_add)

summary(screentime)

#### bullying #####
bully = read.csv("G://data/abcd/release5.1/core/mental-health/mh_y_peq.csv")
bully = bully |> mutate(
  bullying_victim = peq_ss_relational_victim + peq_ss_reputation_victim + peq_ss_overt_victim)

bully = bully |> select(src_subject_id, eventname, bullying_victim)

summary(bully)

#### cyberbullying ####
cyber = read.csv("G://data/abcd/release5.1/core/mental-health/mh_y_cbb.csv")
cyber = cyber |> select(src_subject_id, eventname, cybb_phenx_harm)
cyber$cybb_phenx_harm = na_if(cyber$cybb_phenx_harm, 777)
cyber$cybb_phenx_harm = as.factor(cyber$cybb_phenx_harm)
summary(cyber)

#### chronotype ####
chron = read.csv("G://data/abcd/release5.1/core/physical-health/ph_y_mctq.csv")
chron = chron |> select(src_subject_id, eventname, mctq_msfsc_calc)
summary(chron)

#### early life uncertainty ####
ple = read.csv("G://data/abcd/release5.1/core/mental-health/mh_p_le.csv")
ple = ple |> select(src_subject_id, eventname, ple_p_ss_total_number)
summary(ple)

#### family conflict ####
conflict = read.csv("G://data/abcd/release5.1/core/culture-environment/ce_y_fes.csv")
conflict = conflict |> select(src_subject_id, eventname, fes_y_ss_fc_pr)

summary(conflict)

#### parental monitoring ####
monitor = read.csv("G://data/abcd/release5.1/core/culture-environment/ce_y_pm.csv")
monitor = monitor |> select(src_subject_id, eventname, pmq_y_ss_mean)

summary(monitor)

#### crpbi - relationship w parent ####
crpbi = read.csv("G://data/abcd/release5.1/core/culture-environment/ce_y_crpbi.csv")
crpbi = crpbi |> select(src_subject_id, eventname, crpbi_y_ss_parent)

summary(crpbi)

#### effortful control ####
eff_control = read.csv("G://data/abcd/release5.1/core/mental-health/mh_p_eatq.csv")
eff_control = eff_control |> select(src_subject_id, eventname, eatq_p_ss_effort_cont_ss)

summary(eff_control)

#### parent depression ####
asr = read.csv("G://data/abcd/release5.1/core/mental-health/mh_p_asr.csv")
asr = asr |> select(src_subject_id, eventname, asr_scr_depress_r)

summary(asr)

#### construct full dataset ####
dfs = list(subs_use, ksads, bkfs, erq, sleep, bmi, demog, depriv, commsafety, discrim, activity, screentime, bully, cyber, chron, ple, conflict, monitor, crpbi, eff_control, asr)

dat = purrr::reduce(.x=dfs, merge, by = c("src_subject_id", "eventname"), all = T)

# fill reshist_addr1_adi_perc for all eventnames with baseline values for src_subject_id
dat = dat |> group_by(src_subject_id) |> fill(reshist_addr1_adi_perc, .direction = "updown") |> ungroup()

rm(subs_use, ksads, bkfs, erq, sleep, bmi, demog, depriv, commsafety, discrim, activity, screentime, bully, cyber, chron, ple, conflict, monitor, crpbi, eff_control, asr)

#### add family ID ####
rel_id = read.csv("G://data/abcd/release5.1/core/abcd-general/abcd_y_lt.csv")

dat = merge(dat, rel_id, by=c("src_subject_id", "eventname"), all.x=T)

#### randomly select one individual from each family ####
#set.seed(2024)

#famids_base = dat |> filter(eventname=="baseline_year_1_arm_1") |> select(src_subject_id, rel_family_id)

#fams = unique(famids_base$rel_family_id)
#unrelated = c()

#for (i in 1:length(fams)) {
#  rel_id = fams[i]
#  sub_id = famids_base$src_subject_id[famids_base$rel_family_id==rel_id]
#  keep_id = sample(sub_id, 1)
#  unrelated[i] = keep_id
#}

#saveRDS(unrelated, "G://users/eileen/ABCD/ABCD_Environmental_Risk/ABCDv5.1/DATA/unrelatedIDs.rds")
unrelated = readRDS("G://users/eileen/ABCD/ABCD_Environmental_Risk/ABCDv5.1/DATA/unrelatedIDs.rds")

#### read in CBCL depression and internalising ####
cbcl = read.csv("G://data/abcd/release5.1/core/mental-health/mh_p_cbcl.csv")
cbcl = cbcl |> select(src_subject_id, eventname, cbcl_scr_syn_internal_r, cbcl_scr_dsm5_depress_r)

dat = merge(dat, cbcl, by = c("src_subject_id", "eventname"))
dat$eventname = as.factor(dat$eventname)
dat_unrelated = dat |> filter(src_subject_id %in% unrelated)

saveRDS(dat_unrelated, "G://users/eileen/ABCD/ABCD_Environmental_Risk/ABCDv5.1/DATA/predictors_unrelatedIDs.rds")

saveRDS(dat, "G://users/eileen/ABCD/ABCD_Environmental_Risk/ABCDv5.1/DATA/predictors_allIDs.rds")

#### youth ksads ####
y_ksads = read.csv("G://data/abcd/release5.1/core/mental-health/mh_y_ksads_ss.csv") |> select("src_subject_id", "eventname", "ksads_1_840_t", "ksads_1_841_t", "ksads_1_842_t", "ksads_1_843_t", "ksads_1_844_t", "ksads_1_845_t", "ksads_1_846_t", "ksads_1_847_t") |> filter(src_subject_id %in% unrelated)

names(y_ksads) = c("src_subject_id", "eventname", "mdd_present", "mdd_remission", "mdd_past",
                   "pdd_present", "pdd_remission", "pdd_past", "unspec_present", "unspec_past")

y_ksads = y_ksads |> mutate(
  across(3:ncol(y_ksads), ~as.factor(case_match(.,
                                                0 ~ 0,
                                                1 ~ 1,
                                                c(555, 888) ~ NA
  ))),
)

y_ksads = y_ksads |> mutate(
  mdd_lifetime = as.factor(ifelse(mdd_present==1 | mdd_remission==1 | mdd_past==1, 1, 0)),
  
  any_lifetime = as.factor(ifelse(mdd_present==1 | mdd_remission==1 | mdd_past==1 | pdd_present==1 | pdd_remission==1 | pdd_past==1 | unspec_present==1 | unspec_past==1, 1, 0)))

y_ksads_event = list(
  base = y_ksads |> filter(eventname=="baseline_year_1_arm_1") |> na.omit(),
  y1 = y_ksads |> filter(eventname=="1_year_follow_up_y_arm_1") |> na.omit(),
  y2 = y_ksads |> filter(eventname=="2_year_follow_up_y_arm_1") |> na.omit(),
  y3 = y_ksads |> filter(eventname=="3_year_follow_up_y_arm_1") |> na.omit(),
  y4 = y_ksads |> filter(eventname=="4_year_follow_up_y_arm_1") |> na.omit()
)

saveRDS(y_ksads_event, "G://users/eileen/ABCD/ABCD_Environmental_Risk/ABCDv5.1/DATA/ksads_y.rds")

#### parent ksads ####
p_ksads = read.csv("G://data/abcd/release5.1/core/mental-health/mh_p_ksads_ss.csv") |> select("src_subject_id", "eventname", "ksads_1_840_p", "ksads_1_841_p", "ksads_1_842_p", "ksads_1_843_p", "ksads_1_844_p", "ksads_1_845_p", "ksads_1_846_p", "ksads_1_847_p") |> filter(src_subject_id %in% unrelated)

names(p_ksads) = c("src_subject_id", "eventname", "mdd_present", "mdd_remission", "mdd_past",
                   "pdd_present", "pdd_remission", "pdd_past", "unspec_present", "unspec_past")

p_ksads = p_ksads |> mutate(
  across(3:ncol(p_ksads), ~as.factor(case_match(.,
                                                0 ~ 0,
                                                1 ~ 1,
                                                c(555, 888) ~ NA
  ))),
)

p_ksads = p_ksads |> mutate(
  mdd_lifetime = as.factor(ifelse(mdd_present==1 | mdd_remission==1 | mdd_past==1, 1, 0)),
  
  any_lifetime = as.factor(ifelse(mdd_present==1 | mdd_remission==1 | mdd_past==1 | pdd_present==1 | pdd_remission==1 | pdd_past==1 | unspec_present==1 | unspec_past==1, 1, 0)))

p_ksads_event = list(
  base = p_ksads |> filter(eventname=="baseline_year_1_arm_1") |> na.omit(),
  y1 = p_ksads |> filter(eventname=="1_year_follow_up_y_arm_1") |> na.omit(),
  y2 = p_ksads |> filter(eventname=="2_year_follow_up_y_arm_1") |> na.omit(),
  y3 = p_ksads |> filter(eventname=="3_year_follow_up_y_arm_1") |> na.omit(),
  y4 = p_ksads |> filter(eventname=="4_year_follow_up_y_arm_1") |> na.omit()
)

saveRDS(p_ksads_event, "G://users/eileen/ABCD/ABCD_Environmental_Risk/ABCDv5.1/DATA/ksads_p.rds")
