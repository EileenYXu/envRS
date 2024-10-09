library(tidyverse)

dat = readRDS("G://users/eileen/ABCD/ABCD_Environmental_Risk/ABCDv5.1/DATA/predictors_unrelatedIDs.rds")
y2 = dat %>% filter(eventname=="2_year_follow_up_y_arm_1") %>% select_if(~ !all(is.na(.)))

names(y2)
sapply(y2, function(x)sum(is.na(x))) |> sort(decreasing = T)
       