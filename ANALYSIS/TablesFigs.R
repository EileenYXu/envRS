#########################################
## Making figures + descriptive tables ##
#########################################

renv::load()
here::i_am("ANALYSIS/TablesFigs.R")

# Packages
library(tidyverse)
library(pROC)
library(patchwork)
library(paletteer)

# Figure 2 ----

## Panel A ----
coefs = read.csv("ANALYSIS/OUT/depRS_main_Y2.csv")
coefs = coefs[-1,] |> filter(Sig=="Y")

top10 = coefs |> arrange(desc(abs(Estimate))) |> slice_head(n = 10)
top10

plotlabs = c("Parental depression", "Dieting", "Witnessed DV", "Sleep duration",
             "Parental monitoring", "Age (months)", "Area deprivation index", 
             "Family conflict", "Parental acceptance", "BMI")

top10$labs = plotlabs

plotA = ggplot(top10) +
  geom_pointrange(aes(x = Estimate, xmin = Lower, xmax = Upper, 
                      y = fct_rev(labs), size = abs(Estimate), 
                      colour = Estimate)) +
  geom_vline(xintercept = 0, alpha = 0.3, linetype = "dashed") + 
  scale_size_binned(range = c(0.3, 0.9), guide = NULL, aesthetics = "size") +
  scale_colour_gradientn(colours = viridisLite::turbo(4,begin = 0.45), 
                         guide = NULL)+
  scale_y_discrete(name = NULL) +
  scale_x_continuous(name = "Std. \U03b2 coefficient", 
                     limits = c(-0.1, 0.35)) +
  theme_bw() +
  theme(axis.text.y = element_text(size = 10), legend.title = element_text(size=9))

plotA

## Panel B (parent) ----
rocs.p = readRDS("DATA/main_rocs_p.rds")

parent = ggroc(rocs.p) +
  scale_colour_paletteer_d(`"colorblindr::OkabeIto"`) +
  geom_segment(aes(x=1, y=0, xend=0, yend=1), colour = "black", alpha = 0.7,
               linetype = "dashed") + theme_bw() + 
  labs(x = "Specificity", y = "Sensitivity", color = "Model")

parent

## Panel C (youth) ----
rocs.y = readRDS("DATA/main_rocs_y.rds")

youth = ggroc(rocs.y) +
  scale_colour_paletteer_d(`"colorblindr::OkabeIto"`) +
  geom_segment(aes(x=1, y=0, xend=0, yend=1), colour = "black", alpha = 0.7,
               linetype = "dashed") + theme_bw() + 
  labs(x = "Specificity", y = "Sensitivity", color = "Model")

youth
  
## Assemble ----
coef.plot = plotA + ggtitle("A")
p.plot = parent + ggtitle("B")
y.plot = youth + ggtitle("C")

fig2 = coef.plot + (p.plot / y.plot) + 
  plot_layout(guides = "collect", widths = c(1.3,1))
fig2

ggsave(plot = fig2, filename = "PLOTS/fig2_OkabeIto.jpg", 
       height = 6, width = 9, units = "in", dpi = "retina")

#rm(list = ls())

# Tables -------------

source("ANALYSIS/funs.R")

# full sample (including twins)
abcd = readRDS("DATA/predictors_allIDs.rds")
age_y2 = abcd |> filter(eventname == "y2") |> 
  select(src_subject_id, interview_age) |> set_names(c("src_subject_id", "age_y2"))
abcd = abcd |> filter(eventname == "baseline")
abcd = merge(abcd, age_y2, by = "src_subject_id", all.x = TRUE)

# unrelated sample (pre-imputation)
in_main = readRDS("DATA/predictors_unrelatedIDs.rds") |> 
  filter(eventname == "baseline" & gender != "GNC") |> pull(src_subject_id)

# PRS ancestry groupings
anc = readRDS("DATA/mdd25_prs.rds") |> select(IID, ANC) |> filter(!duplicated(IID))

abcd = merge(abcd, anc, by.x = "src_subject_id", by.y = "IID", all.x = TRUE)

# MDD status
mdd.y = readRDS("DATA/ksads_y.rds")
mdd.p = readRDS("DATA/ksads_p.rds")

mdd.y = mdd.y |> select(src_subject_id, mdd_lifetime, mdd_incident) |> 
  setNames(c("id", "mdd.y"))
mdd.p = mdd.p |> select(src_subject_id, mdd_lifetime, mdd_incident) |> 
  setNames(c("id", "mdd.p"))

mdd.all = merge(mdd.p, mdd.y, by = "id", all = TRUE)

abcd = merge(abcd, mdd.all, all.x = TRUE, by.x = "src_subject_id", by.y = "id")

# Recode age to years and income as factor, put education and race/ethnicity in order for table
abcd = abcd |> mutate(
  age = interview_age/12,
  age_y2 = age_y2/12,
  gender = factor(gender, levels = c("F", "M", "GNC"), ordered = T),
  birthsex = factor(birthsex, levels = c("F", "M", "Intersex"), ordered = T),
  income = as.factor(income),
  race_ethnicity = factor(race_ethnicity,
                          levels = c("Asian", "Black", "Hispanic", "White", "Other"),
                          ordered = TRUE),
  parent_ed = factor(parent_ed,
                     levels = c("less_HS", "HS_GED", "Some_College", "Bachelor",
                                "Postgraduate"), ordered = TRUE))

# Sample included in PRS analyses
depRS_PRS = readRDS("DATA/depRS_PRS.rds")

# exclude baseline MDD
in_PRS_p = depRS_PRS |> filter(!is.na(mdd.p) & incident.p!="exclude") |> 
  pull(src_subject_id)
in_PRS_y = depRS_PRS |> filter(!is.na(mdd.y) & incident.y!="exclude") |> 
  pull(src_subject_id)

rm(list = setdiff(ls(), c("abcd", "in_PRS_p", "in_PRS_y", "in_main", "get_sum_stats")))

## Table 1: Sample demographic characteristics for depRS and PRS analyses ----
vars = c("age", "age_y2", "gender", "birthsex", "race_ethnicity", "income",
         "parent_ed", "ANC", "mdd.p", "mdd.y")

main = abcd |> filter(src_subject_id %in% in_main) |> 
  select(any_of(vars)) |> get_sum_stats(vars = vars)

PRSp = abcd |> filter(src_subject_id %in% in_PRS_p) |> 
  select(any_of(vars)) |> get_sum_stats(vars = vars)

PRSy = abcd |> filter(src_subject_id %in% in_PRS_y) |> 
  select(any_of(vars)) |> get_sum_stats(vars = vars)

table1 = list("depRS main" = main, "PRS parent" = PRSp, "PRS youth" = PRSy)

openxlsx::write.xlsx(table1, "ANALYSIS/OUT/table1.xlsx")

rm(main, PRSp, PRSy)

## Table S1: Demographic characteristics of ABCD participants excluded from analyses ----

main = abcd |> filter(!(src_subject_id %in% in_main)) |> 
  select(any_of(vars)) |> get_sum_stats(vars = vars)

PRSp = abcd |> filter(!(src_subject_id %in% in_PRS_p)) |> 
  select(any_of(vars)) |> get_sum_stats(vars = vars)

PRSy = abcd |> filter(!(src_subject_id %in% in_PRS_y)) |> 
  select(any_of(vars)) |> get_sum_stats(vars = vars)

tableS1 = list("exc main" = main, "exc parent" = PRSp, "exc youth" = PRSy)

openxlsx::write.xlsx(tableS1, "ANALYSIS/OUT/table_S1.xlsx")

rm(main, PRSp, PRSy)

## Table S2: Baseline demographic characteristics of participants included in PRS analyses by lifetime MDD status ----

PRSp_0 = abcd |> filter(src_subject_id %in% in_PRS_p & mdd.p == 0) |> 
  select(any_of(vars)) |> get_sum_stats(vars = vars)

PRSp_1 = abcd |> filter(src_subject_id %in% in_PRS_p & mdd.p == 1) |> 
  select(any_of(vars)) |> get_sum_stats(vars = vars)

PRSy_0 = abcd |> filter(src_subject_id %in% in_PRS_y & mdd.y == 0) |> 
  select(any_of(vars)) |> get_sum_stats(vars = vars)

PRSy_1 = abcd |> filter(src_subject_id %in% in_PRS_y & mdd.y == 1) |> 
  select(any_of(vars)) |> get_sum_stats(vars = vars)

tableS2 = list("MDDp control" = PRSp_0, "MDDp case" = PRSp_1,
               "MDDy control" = PRSy_0, "MDDy case" = PRSy_1)

openxlsx::write.xlsx(tableS2, "ANALYSIS/OUT/table_S2.xlsx")

rm(PRSp_0, PRSp_1, PRSy_0, PRSy_1, in_PRS_p, in_PRS_y)

## Table S5: Demographic characteristics for participants at 2-year follow-up (Sensitivity analysis 3) ----
y2 = readRDS("DATA/dat_y2.rds") |> na.omit() |> filter(gender!="GNC") |> droplevels()
y2 = y2 |> mutate(
  age = interview_age/12,
  gender = factor(gender, levels = c("F", "M", "GNC"), ordered = T),
  birthsex = factor(birthsex, levels = c("F", "M", "Intersex"), ordered = T),
  income = as.factor(income),
  race_ethnicity = factor(race_ethnicity,
                          levels = c("Asian", "Black", "Hispanic", "White", "Other"),
                          ordered = TRUE),
  parent_ed = factor(parent_ed,
                     levels = c("less_HS", "HS_GED", "Some_College", "Bachelor",
                                "Postgraduate"), ordered = TRUE))


y2 = y2 |> select(any_of(vars))
tableS5 = y2 |> get_sum_stats(names(y2))

openxlsx::write.xlsx(tableS5, "ANALYSIS/OUT/table_S5.xlsx")
