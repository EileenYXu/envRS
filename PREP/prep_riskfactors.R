#############################################################################
#### Script for building dataset of depRS predictors from ABCD v5.1 data ####
#############################################################################

renv::load()
here::i_am("PREP/prep_riskfactors.R")
library(tidyverse)

#### tlfb: alcohol, cannabis, polydrug use ####

tlfb = read.csv("abcdv5.1/substance-use/su_y_tlfb.csv") |> filter(
  eventname=="baseline_year_1_arm_1" | eventname=="2_year_follow_up_y_arm_1") |>
  select(src_subject_id, eventname, 
                       tlfb_cal_scr_alc_ud, #number of alcohol use days
                       tlfb_cal_scr_alc_max, #max units consumed in one sitting
                       su_tlfb_cal_scr_mj_days_yr, #number of cannabis use days
                       tlfb_cal_scr_num_events) #number substance use days
names(tlfb)[3:ncol(tlfb)] = c("alc_days", "alc_max_units", "mj_days", "total_su_days")
summary(tlfb) #only 162 answered these questions (i.e. passed gating)

# sui: illicit drug use, tobacco use ####
sui = read.csv("abcdv5.1/substance-use/su_y_sui.csv") |> filter(
  eventname=="baseline_year_1_arm_1" | eventname=="2_year_follow_up_y_arm_1") |>
  select(src_subject_id, eventname, 
         # illicit drugs:
         tlfb_coc_use, tlfb_bsalts_use, tlfb_meth_use, tlfb_mdma_use,
         tlfb_ghb_use, tlfb_opi_use, tlfb_hall_use, tlfb_shrooms_use,
         tlfb_sniff_use,
         # ever smoked tobacco
         tlfb_tob_puff)

# gating means that questions were only administered if yp heard of the drug
# code any illicit drug use - yes (1), no (0)
sui = sui |> mutate(
  illicit = case_when(
    rowSums(na.rm = T, sui[,3:11])>0 ~ 1, 
    .default = 0) |> as.factor())

sui = sui |> mutate(
  illicit = as.factor(illicit),
  tobacco_puff = as.factor(tlfb_tob_puff)) |> select(
    src_subject_id, eventname, illicit, tobacco_puff)

# merge tlfb and sui data
subs_use = merge(sui, tlfb, by = c("src_subject_id", "eventname"), all.x=T)
summary(subs_use)
rm(sui, tlfb)

#### ksads: symptom-weight control, ptsd scale for traumatic events ####
diet = read.csv("abcdv5.1/mental-health/mh_p_ksads_ss.csv") |> filter(
  eventname=="baseline_year_1_arm_1" | eventname=="2_year_follow_up_y_arm_1") |> 
  select(src_subject_id, eventname, ksads_13_72_p)
names(diet)[3] = "weightcontrol_ksads"

ptsd = read.csv("abcdv5.1/mental-health/mh_p_ksads_ptsd.csv") |> filter(
  eventname=="baseline_year_1_arm_1" | eventname=="2_year_follow_up_y_arm_1") |> 
  select(src_subject_id, eventname, ksads_ptsd_raw_760_p, ksads_ptsd_raw_767_p,
         ksads_ptsd_raw_768_p, ksads_ptsd_raw_769_p, ksads_ptsd_raw_761_p,
         ksads_ptsd_raw_762_p, ksads_ptsd_raw_763_p, ksads_ptsd_raw_770_p,
         ksads_ptsd_raw_766_p, ksads_ptsd_raw_764_p, ksads_ptsd_raw_765_p,
         ksads_ptsd_raw_754_p, ksads_ptsd_raw_755_p)

# make abuse and serious accident/injury trauma variables, rename others
ptsd = ptsd |> mutate(
  witness_comm_violence = ksads_ptsd_raw_760_p,
  death_loved_one = ksads_ptsd_raw_770_p,
  witness_dv = ksads_ptsd_raw_766_p,
  s_abuse = case_when(rowSums(na.rm = T, ptsd[,4:6])>0 ~ 1, .default = 0) |> as.factor(),
  p_abuse = case_when(rowSums(na.rm = T, ptsd[,7:9])>0 ~ 1, .default = 0) |> as.factor(),
  emot_abuse = case_when(rowSums(na.rm = T, ptsd[,12:13])>0 ~ 1, .default = 0) |> as.factor(),
  serious_accident = case_when(rowSums(na.rm = T, ptsd[,14:15])>0 ~ 1, .default = 0) |> as.factor()
)

ptsd = ptsd |> select(src_subject_id, eventname, witness_comm_violence, 
                      death_loved_one, witness_dv, s_abuse, p_abuse, 
                      emot_abuse, serious_accident)

# merge dieting and ptsd
ksads = merge(diet, ptsd, by = c("src_subject_id", "eventname"), all.x=T)
ksads[,3:ncol(ksads)] = lapply(ksads[,3:ncol(ksads)], as.factor)
summary(ksads)
rm(diet, ptsd)

#### bkfs ####
bkfs = read.csv("abcdv5.1/physical-health/ph_p_bkfs.csv") |> 
  filter(eventname=="baseline_year_1_arm_1" | eventname=="2_year_follow_up_y_arm_1") |>
  select(src_subject_id, eventname, bkfs_fruit_ce, bkfs_vegnopot_ce, bkfs_dt_fibe)
names(bkfs)[3:5] = c("bkfs_fruit", "bkfs_veg", "bkfs_fiber")
summary(bkfs)

#### erq coping #####
# erq = read.csv("abcdv5.1/mental-health/mh_y_erq.csv") 
# not at baseline or year 2

#### sleep duration ####
sleep = read.csv("abcdv5.1/physical-health/ph_p_sds.csv") |> filter(
  eventname=="baseline_year_1_arm_1" | eventname=="2_year_follow_up_y_arm_1") |> 
  select(src_subject_id, eventname, sleepdisturb1_p) |> 
  mutate(
  sleep_hrs = factor(sleepdisturb1_p, 
                     labels = c("9+hrs", "8-9hrs", "7-8hrs","5-7hrs", "<5hrs"), 
                     ordered = T))

sleep$sleep_hrs = fct_rev(sleep$sleep_hrs)
sleep = sleep |> select(src_subject_id, eventname, sleep_hrs)

#### bmi ####
bmi = read.csv("DATA/BMI_modified_z.csv") |> 
  filter(eventname=="baseline_year_1_arm_1" | eventname=="2_year_follow_up_y_arm_1")

bmi = bmi |> filter(biv=="plausible") |> select(src_subject_id, eventname, bmi)

#### demogs ####
demog = read.csv("abcdv5.1/abcd-general/abcd_p_demo.csv") |> 
  filter(eventname=="baseline_year_1_arm_1" | eventname=="2_year_follow_up_y_arm_1")
demog = demog |> select(src_subject_id, eventname, demo_gender_id_v2, 
                        demo_gender_id_v2_l, demo_sex_v2,
                        demo_fam_exp1_v2, demo_fam_exp1_v2_l,
                        demo_comb_income_v2, demo_comb_income_v2_l,
                        demo_prnt_ed_v2, demo_prnt_ed_v2_2yr_l,
                        demo_prnt_ed_v2_l, race_ethnicity) |> 
  mutate(across(where(is.numeric), ~na_if(.,777)),
         across(where(is.numeric), ~na_if(.,999)))

# fill in missing sex and race/ethnicity variables at y2
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
  needed_food = as.factor(needed_food),
  race_ethnicity = factor(race_ethnicity, 
                          labels = c("White", "Black","Hispanic", 
                                     "Asian", "Other")),
  birthsex = factor(demo_sex_v2, levels = c(1, 2, 3),
                    labels =c("M", "F", "Intersex")),
  gender_id = factor(gender_id, levels = c(1, 2, 3, 4, 5, 6),
                     labels = c("M", "F", "Mtrans",
                                "Ftrans", "GNC", "Diff"))
)

# use case_when for parent education and for gender variable where cis and trans people merged as one
demog = demog |> mutate(
  parent_ed = case_when(
    is.na(parent_ed) ~ NA_character_,
    parent_ed < 13 ~ "less_HS",
    parent_ed == 13 | parent_ed == 14 ~ "HS_GED",
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
depriv = read.csv("abcdv5.1/linked-external-data/led_l_adi.csv") |> 
  filter(eventname=="baseline_year_1_arm_1" | eventname=="2_year_follow_up_y_arm_1") |>
  select(src_subject_id, eventname, reshist_addr1_adi_perc)
names(depriv)[3]="area_depriv"
summary(depriv)

#### community safety ####
commsafety = read.csv("abcdv5.1/culture-environment/ce_p_nsc.csv") |> 
  filter(eventname=="baseline_year_1_arm_1" | eventname=="2_year_follow_up_y_arm_1") |>
  select(src_subject_id, eventname, nsc_p_ss_mean_3_items)
names(commsafety)[3]="comm_safety"
summary(commsafety)

#### discrimination ####
discrim = read.csv("abcdv5.1/culture-environment/ce_y_dm.csv") |> 
  filter(eventname=="baseline_year_1_arm_1" | eventname=="2_year_follow_up_y_arm_1") |> 
  select(src_subject_id, eventname, dim_y_ss_mean)
names(discrim)[3]="discrimination"
summary(discrim)

#### physical activity ####
activity = read.csv("abcdv5.1/physical-health/ph_y_yrb.csv") |> 
  filter(eventname=="baseline_year_1_arm_1" | eventname=="2_year_follow_up_y_arm_1") |> 
  select(src_subject_id, eventname, physical_activity1_y)
names(activity)[3]="days_acti"
summary(activity)

#### screentime ####
screentime = read.csv("abcdv5.1/novel-technologies/nt_y_st.csv") |> filter(eventname=="2_year_follow_up_y_arm_1")
screentime = screentime |> mutate(
  across(where(is.numeric), ~na_if(.,777)),
  soc_media_add = rowSums(screentime[,grep("screentime_smqa", names(screentime))]),
  video_game_add = rowSums(screentime[,grep("screentime_vgaq", names(screentime))]))

screentime = screentime |> select(src_subject_id, eventname, screentime_smq_soc_med_hr, screentime_smq_sm_min, soc_media_add, video_game_add)
summary(screentime)

#### bullying #####
bully = read.csv("abcdv5.1/mental-health/mh_y_peq.csv") |> 
  filter(eventname=="baseline_year_1_arm_1" | eventname=="2_year_follow_up_y_arm_1") |>
  mutate(bullying_victim = peq_ss_relational_victim + 
           peq_ss_reputation_victim + peq_ss_overt_victim) |> 
  select(src_subject_id, eventname, bullying_victim)

summary(bully)

#### cyberbullying ####
cyber = read.csv("abcdv5.1/mental-health/mh_y_cbb.csv") |> 
  filter(eventname=="baseline_year_1_arm_1" | eventname=="2_year_follow_up_y_arm_1") |>
  select(src_subject_id, eventname, cybb_phenx_harm)
cyber$cybb_phenx_harm = na_if(cyber$cybb_phenx_harm, 777) |> as.factor()
names(cyber)[3]="cyberbullying"
summary(cyber)

#### chronotype ####
chron = read.csv("abcdv5.1/physical-health/ph_y_mctq.csv") |> 
  filter(eventname=="baseline_year_1_arm_1" | eventname=="2_year_follow_up_y_arm_1") |>
  select(src_subject_id, eventname, mctq_msfsc_calc)
names(chron)[3]="chronotype"
summary(chron)

#### early life uncertainty ####
ple = read.csv("abcdv5.1/mental-health/mh_p_le.csv") |> 
  filter(eventname=="baseline_year_1_arm_1" | eventname=="2_year_follow_up_y_arm_1") |>
  select(src_subject_id, eventname, ple_p_ss_total_number)
names(ple)[3]="life_events"
summary(ple)

#### family conflict ####
conflict = read.csv("abcdv5.1/culture-environment/ce_y_fes.csv") |> 
  filter(eventname=="baseline_year_1_arm_1" | eventname=="2_year_follow_up_y_arm_1") |>
  select(src_subject_id, eventname, fes_y_ss_fc_pr)
names(conflict)[3]="fam_conflict"
summary(conflict)

#### parental monitoring ####
monitor = read.csv("abcdv5.1/culture-environment/ce_y_pm.csv") |> 
  filter(eventname=="baseline_year_1_arm_1" | eventname=="2_year_follow_up_y_arm_1") |>
  select(src_subject_id, eventname, pmq_y_ss_mean)
names(monitor)[3]="p_monitoring"
summary(monitor)

#### crpbi - relationship w parent ####
crpbi = read.csv("abcdv5.1/culture-environment/ce_y_crpbi.csv") |> 
  filter(eventname=="baseline_year_1_arm_1" | eventname=="2_year_follow_up_y_arm_1") |>
  select(src_subject_id, eventname, crpbi_y_ss_parent)
names(crpbi)[3]="p_acceptance"
summary(crpbi)

#### effortful control ####
eff_control = read.csv("abcdv5.1/mental-health/mh_p_eatq.csv") |> 
  filter(eventname=="baseline_year_1_arm_1" | eventname=="2_year_follow_up_y_arm_1") |>
  select(src_subject_id, eventname, eatq_p_ss_effort_cont_ss)
names(eff_control)[3]="eff_control"
summary(eff_control)

#### parent depression ####
asr = read.csv("abcdv5.1/mental-health/mh_p_asr.csv") |> 
  filter(eventname=="baseline_year_1_arm_1" | eventname=="2_year_follow_up_y_arm_1") |>
  select(src_subject_id, eventname, asr_scr_depress_r)
names(asr)[3]="p_depression"
summary(asr)

#### construct full dataset ####
dfs = list(subs_use, ksads, bkfs, sleep, bmi, demog, depriv, commsafety, discrim, activity, screentime, bully, cyber, chron, ple, conflict, monitor, crpbi, eff_control, asr)

dat = purrr::reduce(.x=dfs, merge, by = c("src_subject_id", "eventname"), all = T)

# fill area_depriv for all eventnames with baseline values for src_subject_id
dat = dat |> group_by(src_subject_id) |> fill(area_depriv, .direction = "updown") |> ungroup()

rm(subs_use, ksads, bkfs, sleep, bmi, demog, depriv, commsafety, discrim, activity, screentime, bully, cyber, chron, ple, conflict, monitor, crpbi, eff_control, asr)
rm(dfs)

#### add family ID ####
rel_id = read.csv("abcdv5.1/abcd-general/abcd_y_lt.csv")
dat = merge(dat, rel_id, by=c("src_subject_id", "eventname"), all.x=T)

#### randomly select one individual from each family, save IDlist ####
set.seed(2024)

famids_base = dat |> filter(eventname=="baseline_year_1_arm_1") |> select(src_subject_id, rel_family_id, site_id_l)

fams = unique(famids_base$rel_family_id)
unrelated = c()

for (i in 1:length(fams)) {
  rel_id = fams[i]
  sub_id = famids_base$src_subject_id[famids_base$rel_family_id==rel_id]
  keep_id = sample(sub_id, 1)
  unrelated[i] = keep_id
}

#saveRDS(unrelated, "DATA/unrelatedIDs.rds")
#unrelated = readRDS("DATA/unrelatedIDs.rds")

#### read in CBCL depression and internalising ####
cbcl = read.csv("abcdv5.1/mental-health/mh_p_cbcl.csv") |> 
  filter(eventname=="baseline_year_1_arm_1" | eventname=="2_year_follow_up_y_arm_1") |>
  select(src_subject_id, eventname, cbcl_scr_syn_internal_r, cbcl_scr_dsm5_depress_r)
names(cbcl)[3:4] = c("cbcl_internalising", "cbcl_dsm5_depress")

dat = merge(dat, cbcl, by = c("src_subject_id", "eventname"))
dat = dat |> mutate(
  eventname = factor(eventname, 
                     levels = c("2_year_follow_up_y_arm_1", "baseline_year_1_arm_1"), 
                     labels = c("y2", "baseline")),
  site_id_l = as.factor(site_id_l))
dat_unrelated = dat |> filter(src_subject_id %in% unrelated)

saveRDS(dat_unrelated, "DATA/predictors_unrelatedIDs.rds")
saveRDS(dat, "DATA/predictors_allIDs.rds")

#### youth ksads ####
y_ksads = read.csv("abcdv5.1/mental-health/mh_y_ksads_ss.csv") |> select("src_subject_id", "eventname", "ksads_1_840_t", "ksads_1_841_t", "ksads_1_842_t", "ksads_1_843_t", "ksads_1_844_t", "ksads_1_845_t", "ksads_1_846_t", "ksads_1_847_t")

names(y_ksads) = c("src_subject_id", "eventname", "mdd_present", "mdd_remission", "mdd_past", "pdd_present", "pdd_remission", "pdd_past", "unspec_present", "unspec_past")

# code missing values
y_ksads = y_ksads |> mutate(across(3:ncol(y_ksads), ~as.factor(
  recode_values(., from = c(0, 1, 555, 888), to = c(0, 1, NA, NA)))))

# code lifetime mdd
y_ksads = y_ksads |> mutate(
  mdd_lifetime = as.factor(
    case_when(
      mdd_present==1 | mdd_remission==1 | mdd_past==1 ~ 1,
      is.na(mdd_present) & is.na(mdd_remission) & is.na(mdd_past) ~ NA,
      .default = 0)))

base = y_ksads |> filter(eventname=="baseline_year_1_arm_1") |>  
  select(src_subject_id, mdd_lifetime) |> setNames(c("src_subject_id", "mdd_b"))

y2 = y_ksads |> filter(eventname=="2_year_follow_up_y_arm_1") |> 
  select(src_subject_id, mdd_lifetime)

y_ksads = merge(base, y2, by = "src_subject_id", all = TRUE) 

y_ksads = y_ksads |> mutate(
  mdd_incident = as.factor(case_when(
    mdd_b=="1" ~ "exclude",
    is.na(mdd_b) ~ NA,
    mdd_lifetime=="1" ~ "1",
    mdd_lifetime=="0" ~ "0",
    is.na(mdd_lifetime) ~ NA
    ))) |> select(src_subject_id, mdd_b, mdd_lifetime, mdd_incident)

summary(y_ksads)

saveRDS(y_ksads, "DATA/ksads_y.rds")

#### parent ksads ####
p_ksads = read.csv("abcdv5.1/mental-health/mh_p_ksads_ss.csv") |> select("src_subject_id", "eventname", "ksads_1_840_p", "ksads_1_841_p", "ksads_1_842_p", "ksads_1_843_p", "ksads_1_844_p", "ksads_1_845_p", "ksads_1_846_p", "ksads_1_847_p")

names(p_ksads) =  c("src_subject_id", "eventname", "mdd_present", "mdd_remission", "mdd_past", "pdd_present", "pdd_remission", "pdd_past", "unspec_present", "unspec_past")

# code missing values
p_ksads = p_ksads |> mutate(
  across(3:ncol(p_ksads), ~as.factor(
    recode_values(., from = c(0, 1, 555, 888), to = c(0, 1, NA, NA)))))

# code lifetime mdd
p_ksads = p_ksads |> mutate(
  mdd_lifetime = as.factor(
    case_when(
      mdd_present==1 | mdd_remission==1 | mdd_past==1 ~ 1,
      is.na(mdd_present) & is.na(mdd_remission) & is.na(mdd_past) ~ NA,
      .default = 0)))

base = p_ksads |> filter(eventname=="baseline_year_1_arm_1") |>  
  select(src_subject_id, mdd_lifetime) |> setNames(c("src_subject_id", "mdd_b"))

y2 = p_ksads |> filter(eventname=="2_year_follow_up_y_arm_1") |> 
  select(src_subject_id, mdd_lifetime)

p_ksads = merge(base, y2, by = "src_subject_id", all = TRUE) 

p_ksads = p_ksads |> mutate(
  mdd_incident = as.factor(case_when(
    mdd_b=="1" ~ "exclude",
    is.na(mdd_b) ~ NA,
    mdd_lifetime=="1" ~ "1",
    mdd_lifetime=="0" ~ "0",
    is.na(mdd_lifetime) ~ NA
  ))) |> select(src_subject_id, mdd_b, mdd_lifetime, mdd_incident)

summary(p_ksads)

saveRDS(p_ksads, "DATA/ksads_p.rds")
