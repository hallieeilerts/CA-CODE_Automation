
###############
#             #
# FULL UPDATE #
#             #
###############

##############
# Set inputs
##############

## Clear environment
rm(list = ls())

## Type of update
simpleUpdate <- TRUE

## Choose age/sex group
# ageGroup <- "00to28"
# ageGroup <- "01to04"
ageGroup <- "05to09"
# ageGroup <- "10to14"
# ageGroup <- "15to19f"
# ageGroup <- "15to19m"

## Years for update
Years <- 2000:2021

## Prepare session
source("./src/prepare-session.R")
source("./src/create-session-variables.R")

## Results date
resDate <- format(Sys.Date(), format="%Y%m%d") # For naming output files
# resDate <- "20230602" # Can set manually if working with past results.

################################################
# Data preparation
################################################

# Keep as scripts, not functions. 
# These files will grow when I incorporate data-prep steps from Diana.
# For squeezing, will need to sample from uncertainty intervals for some single causes. 
# But that can happen in another script or function in uncertainty section.

# Classification keys
source("./src/data-prep/set-cod-list.R")
source("./src/data-prep/set-regions.R")
source("./src/data-prep/set-country-class.R")

# Exposure and outcome data
source("./src/data-prep/prep-goodvr.R")
source("./src/data-prep/prep-prediction-database.R")
if(ageGroup %in% c("00to28","01to04")){source("./src/data-prep/prep-chinanmch.R")}
if(ageGroup %in% c("05to09","10to14","15to19f", "15to19m")){source("./src/data-prep/prep-chinadsp.R")}

# Envelopes
source("./src/data-prep/prep-envelopes.R")
source("./src/data-prep/prep-envelopes-draws.R")

# Single-cause data
source("./src/data-prep/prep-crisis.R")
source("./src/data-prep/prep-hiv.R")
source("./src/data-prep/prep-malaria.R")
source("./src/data-prep/prep-tb.R")
if(ageGroup == "05to09"){source("./src/data-prep/prep-measles.R")}

# Model fit
if(simpleUpdate){
  source("./src/data-prep/prep-model-fit-hmm.R")
  source("./src/data-prep/prep-model-fit-lmm.R")
}

################################################
# Estimation
################################################

if(!simpleUpdate){
  source("src/estimation/prep-model-input-hmm.R")
  source("src/estimation/prep-model-input-lmm.R")
  source("src/estimation/create-parameter-grid-hmm.R")
  source("src/estimation/create-parameter-grid-lmm.R")
  source("src/estimation/select-covariates-hmm.R")
  source("src/estimation/select-covariates-lmm.R")
  # This is the file that contains the results of the formal parameter selection is named Code/LASSO_VA002.txt in Pancho's code. Can't find this file though.
  #source("src/estimation/formal-parameter-selection-hmm.R")
  #source("src/estimation/formal-parameter-selection-lmm.R")
  source("src/estimation/run-bayesian-lasso-hmm.R")
  source("src/estimation/run-bayesian-lasso-lmm.R")
}

################################################
# Prediction
################################################

# Load inputs and functions
source("./src/prediction/prediction_inputs.R")
source("./src/prediction/prediction_functions.R")

# Calculate CSMFs for goodvr and China, save
csmf_GOODVR <- fn_calc_csmf(db_VR, key_ctryclass, key_cod, ctryGrp = "GOODVR", env)
csmf_CHN    <- fn_calc_csmf(db_CHN, key_ctryclass, key_cod, ctryGrp  = "CHN")
write.csv(csmf_GOODVR, paste("./gen/prediction/output/csmf_", ageGroup, "GOODVR.csv", sep=""), row.names = FALSE)
write.csv(csmf_CHN,    paste("./gen/prediction/output/csmf_", ageGroup, "CHN.csv", sep=""), row.names = FALSE)

# Extract covariate values from prediction database
db_pred_HMM <- fn_extract_cov(vxf_HMM, db_pred, key_ctryclass, "HMM")
db_pred_LMM <- fn_extract_cov(vxf_LMM, db_pred, key_ctryclass, "LMM")
write.csv(db_pred_HMM, paste("./gen/prediction/temp/db_pred_", ageGroup, "HMM.csv",sep=""), row.names = FALSE)
write.csv(db_pred_LMM, paste("./gen/prediction/temp/db_pred_", ageGroup, "LMM.csv",sep=""), row.names = FALSE)

# Run prediction function for each year, format
l_csmf_HMM  <- lapply(Years, function(x){fn_call_p1New(x, fit_HMM, db_pred_HMM)})
l_csmf_LMM  <- lapply(Years, function(x){fn_call_p1New(x, fit_LMM, db_pred_LMM)})

# Set malaria fractions
if(ageGroup %in% c("05to09", "10to14")){
  l_csmf_HMM <- lapply(l_csmf_HMM, function(x){fn_cap_mal_frac(x, dth_malaria_5to19, csmf_malaria_01to04HMM)})
  l_csmf_LMM <- lapply(l_csmf_LMM, function(x){fn_set_mal_frac(x)})
}

# Format and save output
csmf <- fn_format_prediction(l_csmf_HMM, l_csmf_LMM)
write.csv(csmf, paste("./gen/prediction/output/csmf_", ageGroup, ".csv",sep=""), row.names = FALSE)

# Remove inputs
rm(db_VR, db_CHN, fit_HMM, fit_LMM, env, key_cod, key_ctryclass, vxf_HMM, vxf_LMM,
   db_pred, dth_malaria_5to19, csmf_malaria_01to04HMM)
# Remove objects created in this section
suppressWarnings(rm(csmf_GOODVR, csmf_CHN, db_pred_HMM, db_pred_LMM, l_csmf_HMM,
                    l_csmf_LMM, csmf))
# Remove functions
rm(list = lsf.str())

################################################
# Squeezing
################################################

# Load inputs and functions
source("./src/squeezing/squeezing_inputs.R")
source("./src/squeezing/squeezing_functions.R")

# Set minimum fractions
source("./src/squeezing/set-min-frac-cd.R")
source("./src/squeezing/set-min-frac-lri.R")

# Prepare modeled countries and China for squeezing
csmf_AddSinglecause <- fn_prepare_sqz(csmf, env, dth_tb, dth_hiv, dth_crisis, dth_meas, minCD, minLRI)
csmf_AddSinglecause_CHN <- fn_prepare_sqz_china(csmf_CHN, env, dth_hiv, dth_crisis, minCD)

# Perform squeezing
if(ageGroup == "05to09"){
  csmf_SqzOthercmpn     <- fn_sqz_othercmpn(csmf_AddSinglecause)
  csmf_SqzOthercmpn_CHN <- fn_sqz_othercmpn_china(csmf_AddSinglecause_CHN)
  csmf_SqzLri           <- fn_sqz_lri(csmf_SqzOthercmpn)
  csmf_SqzCrisisend     <- fn_sqz_crisisend(csmf_SqzLri)
  dth_SqzCrisisepi      <- fn_sqz_crisisepi(csmf_SqzCrisisend)
  dth_SqzCrisisepi_CHN  <- fn_sqz_crisisepi(csmf_SqzOthercmpn_CHN)
  dth_AddMeasepi        <- fn_add_measepi(dth_SqzCrisisepi)
  dth_Sqz <- dth_AddMeasepi
}
if(ageGroup == "10to14"){
  csmf_SqzOthercmpn     <- fn_sqz_othercmpn(csmf_AddSinglecause)
  csmf_SqzOthercmpn_CHN <- fn_sqz_othercmpn_china(csmf_AddSinglecause_CHN)
  csmf_SqzLri           <- fn_sqz_lri(csmf_SqzOthercmpn)
  csmf_SqzCrisisend     <- fn_sqz_crisisend(csmf_SqzLri)
  dth_SqzCrisisepi      <- fn_sqz_crisisepi(csmf_SqzCrisisend)
  dth_SqzCrisisepi_CHN  <- fn_sqz_crisisepi(csmf_SqzOthercmpn_CHN)
  dth_Sqz <- dth_SqzCrisisepi
}
if(ageGroup %in% c("15to19f", "15to19m")){
  csmf_SqzOthercmpn     <- fn_sqz_othercmpn(csmf_AddSinglecause)
  csmf_SqzOthercmpn_CHN <- fn_sqz_othercmpn_china(csmf_AddSinglecause_CHN)
  csmf_SqzCrisisend     <- fn_sqz_crisisend(csmf_SqzLri)
  dth_SqzCrisisepi      <- fn_sqz_crisisepi(csmf_SqzCrisisend)
  dth_SqzCrisisepi_CHN  <- fn_sqz_crisisepi(csmf_SqzOthercmpn_CHN)
  dth_Sqz <- dth_SqzCrisisepi
}

# Format squeezed output
csmf_Sqz <- fn_format_sqz_output(dth_Sqz, dth_SqzCrisisepi_CHN, csmf)

# Combine squeezed output with VR, save
csmf_ALL <- fn_combine_csmf(csmf_Sqz, csmf_GOODVR)
write.csv(csmf_Sqz, paste("./gen/squeezing/output/csmf_", ageGroup, ".csv", sep=""), row.names = FALSE)

# Remove inputs
rm(csmf, csmf_CHN, env, dth_tb, dth_hiv, dth_crisis, dth_meas, minCD, minLRI, key_cod)
# Remove objects created in this section
suppressWarnings(rm(csmf_AddSinglecause, csmf_AddSinglecause_CHN, csmf_SqzOthercmpn, 
                    csmf_SqzLri, csmf_SqzCrisisend, dth_SqzCrisisepi, dth_AddMeasepi,
                    csmf_SqzOthercmpn_CHN, dth_SqzCrisisepi_CHN, dth_Sqz, csmf_Sqz))
# Remove functions
rm(list = lsf.str())

################################################
# Uncertainty
################################################

# Load inputs and functions
source("./src/uncertainty/uncertainty_inputs.R")
source("./src/uncertainty/uncertainty_functions.R")
source("./src/prediction/prediction_functions.R")
source("./src/squeezing/squeezing_functions.R")

# --- Prediction ---#

# Run prediction function with uncertainty for each year, format
draws_csmf_HMM <- lapply(Years, function(x){fn_call_p1New(x, fit_HMM, db_pred_HMM, UNCERTAINTY = TRUE)})
draws_csmf_LMM <- lapply(Years, function(x){fn_call_p1New(x, fit_LMM, db_pred_LMM, UNCERTAINTY = TRUE)})

# Set malaria fractions
if(ageGroup %in% c("05to09", "10to14")){
  draws_csmf_HMM <- fn_nested_lapply(draws_csmf_HMM, function(x){fn_cap_mal_frac(x, dth_malaria_5to19, csmf_malaria_01to04HMM)})
  draws_csmf_LMM <- fn_nested_lapply(draws_csmf_LMM, function(x){fn_set_mal_frac(x)})
}

# --- Draws --- #

# Rearrange predicted fraction draws
draws_csmf_Rearranged_HMM <- fn_rearrange_draws(draws_csmf_HMM)
draws_csmf_Rearranged_LMM <- fn_rearrange_draws(draws_csmf_LMM)

# Create sampling vectors for IGME envelope draws based on number of HMM/LMM draws
# This will ensure that there are the same number of draws from each source
v_sample <- fn_create_sample_vectors(draws_csmf_Rearranged_HMM, draws_csmf_Rearranged_LMM, draws_env)

# Sample from all draws
draws_env_Sampled <- fn_rand_draw_env(draws_env,  v_sample$env)
draws_csmf_Sampled_HMM <- lapply(v_sample$HMM, function(x){ draws_csmf_Rearranged_HMM[[x]] })
draws_csmf_Sampled_LMM <- lapply(v_sample$LMM, function(x){ draws_csmf_Rearranged_LMM[[x]] })

# Combine predicted draws for HMM and LMM
draws_csmf_Sampled <- fn_format_draws(draws_csmf_Sampled_HMM, draws_csmf_Sampled_LMM)

# Randomly assign CSMFs for goodvr for each draw, save
draws_csmf_GOODVR <- lapply(draws_env_Sampled, function(x){ fn_rand_assign_vr(csmf_GOODVR, x, key_cod, CTRYGRP = "GOODVR")})
saveRDS(draws_csmf_GOODVR, file = paste("./gen/uncertainty/output/draws_csmf_", ageGroup, "GOODVR.rds",sep=""))

# Randomly assign CSMFs for China for each draw, squeeze
draws_csmf_CHN    <- lapply(draws_env_Sampled, function(x){ fn_rand_assign_vr(csmf_CHN, x, key_cod, CTRYGRP = "CHN")})

# --- Squeezing --- #

# Prepare modeled countries and China for squeezing
draws_csmf_AddSinglecause     <- lapply(draws_csmf_Sampled, function(x) fn_prepare_sqz(x, env, dth_tb, dth_hiv, dth_crisis, dth_meas, minCD, minLRI))
draws_csmf_AddSinglecause_CHN <- lapply(draws_csmf_CHN, function(x) fn_prepare_sqz_china(x, env, dth_hiv, dth_crisis, minCD))

# Randomly assign single causes for each draw
if(ageGroup == "05to09"){
  draws_csmf_AddSinglecause <- lapply(draws_csmf_AddSinglecause, function(x){ fn_rand_assign_meas(x)})
}
draws_csmf_AddSinglecause <- lapply(draws_csmf_AddSinglecause, function(x){ fn_rand_assign_tb(x)})
draws_csmf_AddSinglecause <- lapply(draws_csmf_AddSinglecause, function(x){ fn_rand_assign_hiv(x)})
draws_csmf_AddSinglecause_CHN <- lapply(draws_csmf_AddSinglecause_CHN, function(x){ fn_rand_assign_hiv(x)})

# Perform squeezing
if(ageGroup == "05to09"){
  draws_csmf_SqzOthercmpn     <- lapply(draws_csmf_AddSinglecause, function(x){ fn_sqz_othercmpn(x)})
  draws_csmf_SqzOthercmpn_CHN <- lapply(draws_csmf_AddSinglecause_CHN, function(x){ fn_sqz_othercmpn_china(x)})
  draws_csmf_SqzLri           <- lapply(draws_csmf_SqzOthercmpn, function(x){ fn_sqz_lri(x)})
  draws_csmf_SqzCrisisend     <- lapply(draws_csmf_SqzLri, function(x){ fn_sqz_crisisend(x)})
  draws_csmf_SqzCrisisepi     <- lapply(draws_csmf_SqzCrisisend, function(x){ fn_sqz_crisisepi(x)})
  draws_dth_SqzCrisisepi_CHN  <- lapply(draws_csmf_SqzOthercmpn_CHN, function(x){ fn_sqz_crisisepi(x)})
  draws_dth_AddMeasepi        <- lapply(draws_csmf_SqzCrisisepi , function(x){ fn_add_measepi(x)})
  draws_dth_Sqz <- draws_dth_AddMeasepi
}
if(ageGroup == "10to14"){
  draws_csmf_SqzOthercmpn     <- lapply(draws_csmf_AddSinglecause, function(x){ fn_sqz_othercmpn(x)})
  draws_csmf_SqzOthercmpn_CHN <- lapply(draws_csmf_AddSinglecause_CHN, function(x){ fn_sqz_othercmpn_china(x)})
  draws_csmf_SqzLri           <- lapply(draws_csmf_SqzOthercmpn, function(x){ fn_sqz_lri(x)})
  draws_csmf_SqzCrisisend     <- lapply(draws_csmf_SqzLri, function(x){ fn_sqz_crisisend(x)})
  draws_csmf_SqzCrisisepi     <- lapply(draws_csmf_SqzCrisisend, function(x){ fn_sqz_crisisepi(x)})
  draws_dth_SqzCrisisepi_CHN  <- lapply(draws_csmf_SqzOthercmpn_CHN, function(x){ fn_sqz_crisisepi(x)})
  draws_dth_Sqz <- draws_csmf_SqzCrisisepi
}
if(ageGroup %in% c("15to19f", "15to19m")){
  draws_csmf_SqzOthercmpn     <- lapply(draws_csmf_AddSinglecause, function(x){ fn_sqz_othercmpn(x)})
  draws_csmf_SqzOthercmpn_CHN <- lapply(draws_csmf_AddSinglecause_CHN, function(x){ fn_sqz_othercmpn_china(x)})
  draws_csmf_SqzCrisisend     <- lapply(draws_csmf_SqzOthercmpn, function(x){ fn_sqz_crisisend(x)})
  draws_csmf_SqzCrisisepi     <- lapply(draws_csmf_SqzCrisisend, function(x){ fn_sqz_crisisepi(x)})
  draws_dth_SqzCrisisepi_CHN  <- lapply(draws_csmf_SqzOthercmpn_CHN, function(x){ fn_sqz_crisisepi(x)})
  draws_dth_Sqz <- draws_csmf_SqzCrisisepi
}

# Format squeezed output
draws_csmf_Sqz <- mapply(function(x,y,z) fn_format_sqz_output(x,y,z), draws_dth_Sqz, draws_dth_SqzCrisisepi_CHN, draws_csmf_Sampled, SIMPLIFY = FALSE)

# Combine squeezed draws with randomly sampled VR, save
draws_csmf_ALL <- mapply(function(x,y) fn_combine_all_csmf(x,y), draws_csmf_Sqz, draws_csmf_GOODVR, SIMPLIFY = FALSE)
saveRDS(draws_csmf_ALL, file = paste("./gen/uncertainty/output/draws_csmf_", ageGroup, ".rds",sep=""))

# Remove inputs
rm(fit_HMM, fit_LMM, dth_malaria_5to19, csmf_malaria_01to04HMM, db_pred_HMM, db_pred_LMM,
   csmf, csmf_GOODVR, csmf_CHN, env, dth_tb, dth_hiv, dth_crisis, dth_meas, minCD, minLRI,
   draws_env, key_cod)
# Remove objects created in this section
suppressWarnings(rm(draws_csmf_HMM, draws_csmf_LMM, 
                    draws_csmf_Rearranged_HMM, draws_csmf_Rearranged_LMM, 
                    v_sample, draws_env_Sampled, 
                    draws_csmf_Sampled_HMM, draws_csmf_Sampled_LMM, draws_csmf_Sampled,
                    draws_csmf_GOODVR, draws_csmf_CHN, 
                    draws_csmf_AddSinglecause, draws_csmf_AddSinglecause_CHN,
                    draws_csmf_SqzOthercmpn, draws_csmf_SqzLri, 
                    draws_csmf_SqzCrisisend, draws_dth_SqzCrisisepi, draws_dth_AddMeasepi,
                    draws_csmf_SqzOthercmpn_CHN, draws_dth_SqzCrisisepi_CHN, 
                    draws_dth_Sqz, draws_csmf_Sqz))

################################################
# Results
################################################

# Load inputs and functions
source("./src/results/results_inputs.R")
source("./src/results/results_functions.R")












# Format and save results
csmf_Formatted <- fn_format_results(csmf_ALL, key_region, key_ctryclass, codAll)
write.csv(csmf_Formatted, paste("./gen/results/output/PointEstimates_National_", ageGroup,"_", format(Sys.Date(), format="%Y%m%d"),".csv", sep=""), row.names = FALSE)

# Calculate regional estimates, format and save results
csmf_Formatted_REGION <- fn_calc_region(csmf_Formatted, codAll)
write.csv(csmf_Formatted_REGION, paste("./gen/results/output/PointEstimates_Regional_", ageGroup,"_", format(Sys.Date(), format="%Y%m%d"),".csv", sep=""), row.names = FALSE)

# Visualizations
fn_compare_csmf(csmf_Formatted, csmf_OldResults, sample = c("AFG", "BRA", "CHN", "IND", "MEX", "NGA", "SDN"))
fn_compare_csmf(csmf_Formatted_REGION, csmf_OldResults_REGION, regional = TRUE)
# fn_plotrates

# Calculate aggregate age groups, format and save results
# (Need to have already produced results for all standard age groups)
source("./src/results/calculate-agg-agegrp.R")

# Remove inputs
rm(csmf_Sqz, csmf_VR, csmf_ALL, csmf_OldResults, csmf_OldResults_REGION,
   key_cod, key_region, key_ctryclass, codAll)
# Remove objects created in this section
suppressWarnings(rm(csmf_Formatted, csmf_Formatted_REGION))
# Remove functions
rm(list = lsf.str())






################################################################################
#' When to use a script versus a function
#' Script:
#' --things that are subject to change frequently (data prep, manipulating data input)
#' --things that take a long time to do. have a script that saves output as file. could also have function that saves output file...
#' --beginning of a section, so that section can be run independently (all scripts can be run independently which is a benefit)
#' Function:
#' --code that we want to standard and use repeatedly (on different country groupings, in the uncertainty process)
#' --things that don't take long to do
#' --can't be run independently


v_ageGroup <- c("05to09", "10to14", "15to19f", "15to19m")
for(i in 1:length(v_ageGroup)){
  ## Choose age/sex group
  ageGroup <- v_ageGroup[i]
  print(ageGroup)
  ## Years for update
  Years <- 2000:2021
  ## Prepare session
  source("./src/prepare-session.R")
  source("./src/create-session-variables.R")
  ## Testing functions
  source("./src/data-prep/prep-hiv.R")
}
