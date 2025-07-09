## Longitudinal Prediction of Adolescent Depression From Environmental and Polygenic Risk Scores

**[Now available as a pre-print on medRxiv](https://www.medrxiv.org/content/10.1101/2025.07.08.25331098v1)**

**Aim**: To develop a model predicting adolescent depression symptoms (depRS) from a review of environmental risk factors and assess depRS prediction of lifetime depression at 2-year follow-up compared to polygenic risk scores (PRS). 

### ./SCRIPTS/

`summarystats.R` - contains the generic function `get_sum_stats()` used for generating summary tables of sample demographics.\
`demog_tables.R` - uses `for` loops to generate sample demographic tables for participants included and excluded in each analysis.\
`paper_figs.Rmd` - makes figures included in the manuscript.\

### ./SCRIPTS/PREP/

`anthro_BMI_modzscores.R` - cleans ABCD height and weight data, calculates BMI z-scores and identifies biologically implausible values using the `growthcleanr` package.\
`PRS_PCs.R` - merges ancestry PCs with PRS data.\
`prep_riskfactors.R` - extracts environmental risk factors used in depRS models and in sensitivity analyses.\
`Prep_baselinedat.Rmd` and `Prep_y2dat.Rmd` clean risk factor data and remove participants with missing data.\

### ./SCRIPTS/ANALYSIS/

`envRS_long.Rmd` - Elastic Net models predicting CBCL depression scores and CBCL internalising scores at 2-year follow-up using baseline risk factors.\
`envRS_cross.Rmd` - Elastic Net models predicting concurrent CBCL depression and internalising scores.\
`compare_long_cross.Rmd` - Comparing coefficients between longitudinal and concurrent depRS models.\
`predictors_desc.Rmd` - Descriptives for depRS features.\
`depRS_PRS_main.Rmd` - depRS and PRS prediction of parent- and youth-reported lifetime MDD at 2-year follow-up, using PRS from diverse GWAS sumstats.\
`depRS_PRS_supp.Rmd` - Sensitivity analysis using PRS from diverse GWAS sumstats for all ancestries except EUR.\

### ./SCRIPTS/ANALYSIS/Sensitivity/

Contains additional sensitivity analyses predicting CBCL depression and internalising scores based on assigned sex at birth (`sens_envRS_`) and using risk factors from the 2-year follow-up assessment (`envRS_y2`).
