#------------------------------
# This script tidies ABCD height and weight data, calculates BMI and identifies BMI outliers
#------------------------------
# in ABCD v5.1 data, the "anthroheightcalc" and "anthroweightcalc" variables are supposed to be mean height and weight calculations for each participant
# 2 measurements were taken each time, with a third taken if there are big differences between measurements 1 and 2
# mean calculations are supposed to be based on the two closest measurements, but seems to be affected by data entry errors
# e.g. one ppt has 3 height measurements: 64.00, 641.00 and 65.0 lbs. The mean height over the 2 closest measurements should be 64.5, but "anthroheightcalc" says 256.66667.
#------------------------------
# useful references are the 'growthcleanr' package and CDC growth charts, particularly the page on biologically implausible values (BIV) https://www.cdc.gov/nccdphp/dnpao/growthcharts/resources/sas.htm#Extreme



library(tidyverse)
#install.packages("growthcleanr")

abcd = read.csv("G://data/abcd/release5.1/core/physical-health/ph_y_anthro.csv")

dat = data.frame()

for (ppt in 1:nrow(abcd)) {
  src_subject_id = abcd$src_subject_id[ppt]
  eventname = abcd$eventname[ppt]
  
  #fix mean height calculation by identifying the measurement pair with the smallest difference
  ht = c(abcd$anthro_1_height_in[ppt], abcd$anthro2heightin[ppt], abcd$anthro3heightin[ppt]) %>% na_if(0)
  htdiffs = c(abs(ht[1]-ht[2]), abs(ht[1]-ht[3]), abs(ht[2]-ht[3]))
  htmissing = sum(is.na(ht))
  
  if (htmissing>=1) {
    height_m = mean(ht, na.rm=T)
  } else if (min(htdiffs)==htdiffs[1]) {
    height_m = mean(c(ht[1], ht[2]))
  } else if (min(htdiffs)==htdiffs[2]) {
    height_m = mean(c(ht[1], ht[3]))
  } else {
    height_m = mean(c(ht[2], ht[3]))
  }

  # fix mean weight calculation by identifying the measurement pair with smallest difference
  wt = c(abcd$anthroweight1lb[ppt], abcd$anthroweight2lb[ppt], abcd$anthroweight3lb[ppt]) %>% na_if(0)
  wtdiffs = c(abs(wt[1]-wt[2]), abs(wt[1]-wt[3]), abs(wt[2]-wt[3]))
  wtmissing = sum(is.na(wt))

  if (wtmissing>=1) {
    weight_m = mean(wt, na.rm=T)
  } else if (min(wtdiffs)==wtdiffs[1]) {
    weight_m = mean(c(wt[1], wt[2]))
  } else if (min(wtdiffs)==wtdiffs[2]) {
    weight_m = mean(c(wt[1], wt[3]))
  } else {
    weight_m = mean(c(wt[2], wt[3]))
  }
  
  # calculate BMI based on mean height and mean weight, assigning NA to any with missing height/weight data or if mean height/weight is 0 (obvious measurement error)
  if (is.na(weight_m)==T | is.na(height_m)==T) {
    bmi = NA
    } else {bmi = weight_m/I(height_m^2) * 703}
  
  datppt = cbind(src_subject_id, eventname, weight_m, height_m, bmi)
  dat = rbind(dat, datppt)
}

agemths = read.csv("G://data/abcd/release5.1/core/abcd-general/abcd_y_lt.csv")
agemths = agemths %>% select(src_subject_id, eventname, interview_age)
demog = read.csv("G://data/abcd/release5.1/core/abcd-general/abcd_p_demo.csv")
demog = demog %>% filter(eventname=="baseline_year_1_arm_1") %>% select(src_subject_id, demo_sex_v2)

abcd = merge(dat, agemths, by = c("src_subject_id", "eventname"), all.x = T)
abcd = merge(abcd, demog, by = "src_subject_id", all.x = T)

# unfortunately the CDC doesn't let you be intersex, so all 3s need to be converted to NA
abcd$demo_sex_v2 = na_if(abcd$demo_sex_v2, 3)

# also convert height and weight to sensible units 
abcd = abcd %>% mutate(
  wt_kg = as.numeric(weight_m) * 0.45359237,
  ht_cm = as.numeric(height_m) * 2.54,
  sex = demo_sex_v2,
  weight_m = as.numeric(weight_m),
  height_m = as.numeric(height_m),
  bmi = as.numeric(bmi)
)

# use growthcleanr to get modified BMI z scores
modscores = growthcleanr::ext_bmiz(data = abcd, 
                                   age = "interview_age",
                                   wt = "wt_kg",
                                   ht = "ht_cm",
                                   bmi = "bmi",
                                   adjust.integer.age = T)

# identify outliers with modified z scores based on https://www.cdc.gov/nccdphp/dnpao/growthcharts/resources/sas.htm#Extreme (reference to NHANES paper on biologically implausible values (BIVs))
# use low cut-off of -5 and high cut-off of +8, also test high cut-off of +9 and +10SD
# convert age to years for comparison with NHANES
modscores = modscores %>% mutate(
  biv = ifelse(mod_bmiz <= -5, "low biv", ifelse(mod_bmiz>8, "high biv", "plausible")) %>% as.factor(),
  biv_9SD = ifelse(mod_bmiz <= -5, "low biv", ifelse(mod_bmiz>9, "high biv", "plausible")) %>% as.factor(),
  biv_10SD = ifelse(mod_bmiz <= -5, "low biv", ifelse(mod_bmiz>10, "high biv", "plausible")) %>% as.factor(),
  age_yr = age/12
  )

# check how many bivs
modscores %>% select(biv, biv_9SD, biv_10SD) %>% summary()

modscores %>% select(src_subject_id, age_yr, wt, ht, bmi, sex, mod_bmiz, biv, biv_9SD, biv_10SD) %>%   filter(biv == "high biv") %>% summary()
modscores %>% select(src_subject_id, age_yr, wt, ht, bmi, sex, mod_bmiz, biv, biv_9SD, biv_10SD) %>%   filter(biv_9SD == "high biv") %>% summary()
modscores %>% select(src_subject_id, age_yr, wt, ht, bmi, sex, mod_bmiz, biv, biv_9SD, biv_10SD) %>%   filter(biv_10SD == "high biv") %>% summary()

# difference between +8 and +9
modscores %>% select(src_subject_id, age_yr, wt, ht, bmi, sex, mod_bmiz, biv, biv_9SD, biv_10SD) %>% filter(biv=="high biv" & biv_9SD=="plausible")

# difference between +8 and +10
modscores %>% select(src_subject_id, age_yr, wt, ht, bmi, sex, mod_bmiz, biv, biv_9SD, biv_10SD) %>% filter(biv=="high biv" & biv_10SD=="plausible")

# difference between +9 and +10
modscores %>% select(src_subject_id, age_yr, wt, ht, bmi, sex, mod_bmiz, biv, biv_9SD, biv_10SD) %>% filter(biv_9SD=="high biv" & biv_10SD=="plausible")

#write.csv(modscores, "G://users/eileen/ABCD/ABCD_Environmental_Risk/ABCDv5.1/DATA/BMI_modified_z.csv")
