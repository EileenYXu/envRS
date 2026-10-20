## Prep Y2 data, no imputation ##

renv::load()

## packages ----
library(tidyverse)

## read in Y2 data ----
dat = readRDS("DATA/predictors_unrelatedIDs.rds")

## tidy data ---- 
y2 = dat |> filter(eventname=="y2") |> 
  select_if(~ !all(is.na(.))) |> droplevels() 
y2vars = names(y2)

y2 = y2 |> mutate(total_su_days = replace_na(total_su_days, 0))

# remove predictors with >20% missing data, illicit drug use and unused 
# variables - interview details, gender_id 
y2 = y2 |> select_if(~ sum(is.na(.))<0.2*nrow(y2)) |>
  select(-c(eventname, illicit, rel_birth_id, 
            interview_date, visit_type, gender_id)) |> droplevels()

y2vars[y2vars %in% names(y2)==F] # checking which were removed
# [1] "eventname"                 "illicit"                   "alc_days"                 
# [4] "alc_max_units"             "mj_days"                   "gender_id"                
# [7] "screentime_smq_soc_med_hr" "screentime_smq_sm_min"     "soc_media_add"            
# [10] "video_game_add"            "rel_birth_id"              "school_id"  
# [13] "district_id"               "interview_date"            "visit_type"    

# remove cbcl missing data
y2 = y2 |> filter(!is.na(cbcl_internalising))

saveRDS(y2, "DATA/dat_y2.rds")