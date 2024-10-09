library(tidyverse)

dat = readRDS("G://users/eileen/ABCD/ABCD_Environmental_Risk/ABCDv5.1/DATA/predictors_unrelatedIDs.rds")
baseline = dat %>% filter(eventname=="baseline_year_1_arm_1") %>% select_if(~ !all(is.na(.))) |> droplevels()

summary(baseline)

# removing partner_ed since >1000 missing
baseline = baseline %>% select(src_subject_id, tlfb_tob_puff, ksads_13_72_p, ksads_ptsd_raw_760_p, ksads_ptsd_raw_770_p, ksads_ptsd_raw_766_p, sa, pa, ea, serious_accident, sleep_hrs, bmi, needed_food, income, parent_ed, reshist_addr1_adi_perc, nsc_p_ss_mean_3_items, physical_activity1_y, cbcl_scr_syn_internal_r, cbcl_scr_dsm5_depress_r)

names(baseline) = c("src_subject_id", "tobacco_puff", "weightcontrol_ksads", "witness_comm_violence", "death_loved_one", "witness_dv", "s_abuse", "p_abuse", "emot_abuse", "serious_accident", "sleep_hrs", "bmi", "needed_food", "family_income", "parent_ed","area_depriv", "comm_safety", "days_active", "cbcl_internalising", "cbcl_dsm5_depress")

# remove empty levels
baseline = droplevels(baseline)
baseline = baseline %>% mutate(
  tobacco_puff = replace_na(tobacco_puff, "0"),
  family_income = as.ordered(family_income)
)

sapply(baseline, function(x) sum(is.na(x)))

# remove cbcl missing data
baseline = baseline %>% filter(!is.na(cbcl_internalising))

## split into train and test samples ##
#same 2 subjects are missing cbcl data: "NDAR_INV9PVR76W7" "NDAR_INVJHJDGEFN"
index = caret::createDataPartition(baseline$cbcl_internalising[which(is.na(baseline$cbcl_internalising)==F)], 
                            p = 0.2, list = F, times = 1)
traindat = baseline[-index,]
testdat = baseline[index,]

saveRDS(traindat, "G://users/eileen/ABCD/ABCD_Environmental_Risk/ABCDv5.1/DATA/baseline_traindat.rds")
saveRDS(testdat, "G://users/eileen/ABCD/ABCD_Environmental_Risk/ABCDv5.1/DATA/baseline_testdat.rds")
