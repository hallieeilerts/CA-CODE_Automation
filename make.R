
################################################
# Prepare session
################################################

# Clear environment
rm(list = ls())

# Clear files in /gen
#source("./src/prepare-session/clean-gen.R")

# Set ageGroup and Years for estimation
source("./src/prepare-session/set-inputs.R")

# Create session variables for inputs
source("./src/prepare-session/create-session-variables.R")

################################################
# Data management
################################################

# Classification keys
source("./src/data-management/set-cod-list.R")
source("./src/data-management/set-regions.R")
source("./src/data-management/set-country-class.R")

# Envelopes
source("./src/data-management/prep-envelopes.R")
source("./src/data-management/prep-envelopes-draws.R") # For 15-19f or 15-19m, creates draws for males, females, and both sexes combined

# Exposure data
source("./src/data-management/prep-prediction-database.R")

# Outcome data
source("./src/data-management/prep-goodvr.R")
if(ageGroup %in% c("00to28","01to04")){source("./src/data-management/prep-chinanmch.R")}
if(ageGroup %in% c("05to09","10to14","15to19f", "15to19m")){source("./src/data-management/prep-chinadsp.R")}

# Single-cause data
source("./src/data-management/prep-crisis.R")
source("./src/data-management/prep-hiv.R")
source("./src/data-management/prep-malaria.R")
source("./src/data-management/prep-tb.R")
if(ageGroup == "05to09"){source("./src/data-management/prep-measles.R")}

# Model objects
if(simpleUpdate){
  source("./src/data-management/prep-model-objects-hmm.R")
  source("./src/data-management/prep-model-objects-lmm.R")
}

# Set fractions for capping malaria, and mminimum fractions for squeezing
source("./src/data-management/set-frac-cap-malaria.R")
source("./src/data-management/set-frac-min-cd.R")
source("./src/data-management/set-frac-min-lri.R")

# Clear environment
rm(list = ls())

################################################
# Estimation
################################################

# Load inputs and functions
source("./src/estimation/estimation_inputs.R")
source("./src/estimation/estimation_functions.R")

if(!simpleUpdate){
  # Note: Need to update header of all these scripts.
  source("src/estimation/prep-model-input-hmm.R")
  source("src/estimation/prep-model-input-lmm.R")
  source("src/estimation/create-parameter-grid-hmm.R")
  source("src/estimation/create-parameter-grid-lmm.R")
  source("src/estimation/select-covariates-hmm.R")
  source("src/estimation/select-covariates-lmm.R")
  # The file that contains the results of the formal parameter selection is named Code/LASSO_VA002.txt in Pancho's code. Can't find this file though.
  #source("src/estimation/formal-parameter-selection-hmm.R")
  #source("src/estimation/formal-parameter-selection-lmm.R")
  source("src/estimation/run-bayesian-lasso-hmm.R")
  source("src/estimation/run-bayesian-lasso-lmm.R")
}

# Clear environment
rm(list = ls())

################################################
# Prediction
################################################

# Load inputs and functions
source("./src/prediction/prediction_inputs.R")
source("./src/prediction/prediction_functions.R")

# Calculate CSMFs for goodvr and China, save
csmf_GOODVR <- fn_calcCSMF(dat_VR,  key_ctryclass, key_cod,  CTRYGRP = "GOODVR")
csmf_CHN    <- fn_calcCSMF(dat_CHN, key_ctryclass, key_cod,  CTRYGRP  = "CHN")
write.csv(csmf_GOODVR, paste("./gen/prediction/output/csmf_", ageGroup, "GOODVR.csv", sep=""), row.names = FALSE)
write.csv(csmf_CHN,    paste("./gen/prediction/output/csmf_", ageGroup, "CHN.csv", sep=""), row.names = FALSE)

# Extract covariate values from prediction database, save
dat_pred_HMM <- fn_extractCov(mod_covNames_HMM, dat_pred, key_ctryclass, CTRYGRP = "HMM")
dat_pred_LMM <- fn_extractCov(mod_covNames_LMM, dat_pred, key_ctryclass, CTRYGRP = "LMM")
write.csv(dat_pred_HMM, paste("./gen/prediction/temp/dat_pred_", ageGroup, "HMM.csv",sep=""), row.names = FALSE)
write.csv(dat_pred_LMM, paste("./gen/prediction/temp/dat_pred_", ageGroup, "LMM.csv",sep=""), row.names = FALSE)

# Run prediction function for each year, format
l_csmf_HMM  <- lapply(Years, function(x){fn_callP1New(x, mod_fit_HMM, dat_pred_HMM)})
l_csmf_LMM  <- lapply(Years, function(x){fn_callP1New(x, mod_fit_LMM, dat_pred_LMM)})

# Set malaria fractions
if(ageGroup %in% c("05to09", "10to14")){
  l_csmf_HMM <- lapply(l_csmf_HMM, function(x){fn_capMalFrac(x, dat_malaria_5to19, frac_malaria_01to04)})
  l_csmf_LMM <- lapply(l_csmf_LMM, function(x){fn_setMalFrac(x)})
}

# Format predicted CSMFs, save
csmf <- fn_formatPrediction(l_csmf_HMM, l_csmf_LMM)
write.csv(csmf, paste("./gen/prediction/output/csmf_", ageGroup, ".csv",sep=""), row.names = FALSE)

# Clear environment
rm(list = ls())

################################################
# Squeezing
################################################

# Load inputs and functions
source("./src/squeezing/squeezing_inputs.R")
source("./src/squeezing/squeezing_functions.R")

# Merge on envelopes
csmf_envADD        <- fn_mergeEnv(csmf, env)
csmf_envADD_CHN    <- fn_mergeEnv(csmf_CHN, env)
csmf_envADD_GOODVR <- fn_mergeEnv(csmf_GOODVR, env)

# Prepare modeled countries and China for squeezing
csmf_singlecauseADD <- fn_prepareSqz(csmf_envADD, dat_tb, dat_hiv, dat_crisis, dat_meas, frac_cd, frac_lri)
csmf_singlecauseADD_CHN <- fn_prepareSqzChina(csmf_envADD_CHN, dat_hiv, dat_crisis, frac_cd)

# Perform squeezing
if(ageGroup == "05to09"){
  csmf_othercmpnSQZ     <- fn_sqzOtherCMPN(csmf_singlecauseADD)
  csmf_lriSQZ           <- fn_sqzLRI(csmf_othercmpnSQZ)
  csmf_crisisEndSQZ     <- fn_sqzCrisisEnd(csmf_lriSQZ, key_cod)
  dth_crisisEpiSQZ      <- fn_sqzCrisisEpi(csmf_crisisEndSQZ, key_cod)
  dth_measEpiADD        <- fn_addMeasEpi(dth_crisisEpiSQZ)
  dth_SQZ <- dth_measEpiADD
  csmf_othercmpnSQZ_CHN <- fn_sqzOtherCMPNchina(csmf_singlecauseADD_CHN)
  dth_crisisEpiSQZ_CHN  <- fn_sqzCrisisEpi(csmf_othercmpnSQZ_CHN, key_cod)
  dth_SQZ_CHN <- dth_crisisEpiSQZ_CHN
}
if(ageGroup == "10to14"){
  csmf_othercmpnSQZ     <- fn_sqzOtherCMPN(csmf_singlecauseADD)
  csmf_lriSQZ           <- fn_sqzLRI(csmf_othercmpnSQZ)
  csmf_crisisEndSQZ     <- fn_sqzCrisisEnd(csmf_lriSQZ, key_cod)
  dth_crisisEpiSQZ      <- fn_sqzCrisisEpi(csmf_crisisEndSQZ, key_cod)
  dth_SQZ <- dth_crisisEpiSQZ
  csmf_othercmpnSQZ_CHN <- fn_sqzOtherCMPNchina(csmf_singlecauseADD_CHN)
  dth_crisisEpiSQZ_CHN  <- fn_sqzCrisisEpi(csmf_othercmpnSQZ_CHN, key_cod)
  dth_SQZ_CHN <- dth_crisisEpiSQZ_CHN
}
if(ageGroup %in% c("15to19f", "15to19m")){
  csmf_othercmpnSQZ     <- fn_sqzOtherCMPN(csmf_singlecauseADD)
  csmf_crisisEndSQZ     <- fn_sqzCrisisEnd(csmf_othercmpnSQZ, key_cod)
  dth_crisisEpiSQZ      <- fn_sqzCrisisEpi(csmf_crisisEndSQZ, key_cod)
  dth_SQZ <- dth_crisisEpiSQZ
  csmf_othercmpnSQZ_CHN <- fn_sqzOtherCMPNchina(csmf_singlecauseADD_CHN)
  dth_crisisEpiSQZ_CHN  <- fn_sqzCrisisEpi(csmf_othercmpnSQZ_CHN, key_cod)
  dth_SQZ_CHN <- dth_crisisEpiSQZ_CHN
}

# Format squeezed output
csmf_SQZ <- fn_formatSqzOutput(dth_SQZ, dth_SQZ_CHN, csmf_envADD, key_cod)

# Audit: check if squeezed CSMFs add up to 1 or contain NA
csmf_SQZ_AUD <- fn_checkCSMFsqz(csmf_SQZ, key_cod)
if(nrow(csmf_SQZ_AUD) > 0){
  write.csv(csmf_SQZ_AUD, paste("./gen/squeezing/audit/csmf_SQZ_AUD_", ageGroup,".csv", sep=""), row.names = FALSE)
}

# Combine squeezed output from modeled countries (HMM and LMM) and China with GOODVR, format, save
csmfSqz <- rbind(csmf_SQZ, csmf_envADD_GOODVR)
csmfSqz <- fn_formatAllOutput(csmfSqz, key_cod)
write.csv(csmfSqz, paste("./gen/squeezing/output/csmfSqz_", ageGroup, ".csv", sep=""), row.names = FALSE)

# Calculate regional CSMFs
csmfSqz_REG <- fn_calcRegion(csmfSqz, key_cod, key_region)
write.csv(csmfSqz_REG, paste("./gen/squeezing/output/csmfSqz_", ageGroup, "REG.csv", sep=""), row.names = FALSE)

# Clear environment
rm(list = ls())

################################################
# Uncertainty
################################################

# Load inputs and functions
source("./src/uncertainty/uncertainty_inputs.R")
source("./src/uncertainty/uncertainty_functions.R")
source("./src/prediction/prediction_functions.R")
source("./src/squeezing/squeezing_functions.R")

## Prediction

# Run prediction function with uncertainty, format
csmfDraws_HMM <- lapply(Years, function(x){fn_callP1New(x, mod_fit_HMM, dat_pred_HMM, UNCERTAINTY = TRUE) })
csmfDraws_LMM <- lapply(Years, function(x){fn_callP1New(x, mod_fit_LMM, dat_pred_LMM, UNCERTAINTY = TRUE) })

# Set malaria fractions
if(ageGroup %in% c("05to09", "10to14")){
  csmfDraws_HMM <- fn_nestedLapply(csmfDraws_HMM, function(x){fn_capMalFrac(x, dat_malaria_5to19, frac_malaria_01to04HMM) })
  csmfDraws_LMM <- fn_nestedLapply(csmfDraws_LMM, function(x){fn_setMalFrac(x) })
}

## Draws

# Rearrange predicted fraction draws
csmfDraws_FRMT_HMM <- fn_rearrangeDraws(csmfDraws_HMM)
csmfDraws_FRMT_LMM <- fn_rearrangeDraws(csmfDraws_LMM)

# Create sampling vectors for IGME envelope draws based on number of HMM/LMM draws
# This will ensure that there are the same number of draws from each source
v_sample <- fn_createSampleVectors(csmfDraws_FRMT_HMM, csmfDraws_FRMT_LMM, envDraws)
# Save temporary file for use in calculating aggregate age groups
saveRDS(v_sample, file = paste("./gen/uncertainty/temp/sampleDraws.rds", sep=""))

# Sample from all draws
envDraws_SAMP <- fn_randDrawEnv(envDraws,  v_sample$env)
csmfDraws_SAMP_HMM <- lapply(v_sample$HMM, function(x){ csmfDraws_FRMT_HMM[[x]] })
csmfDraws_SAMP_LMM <- lapply(v_sample$LMM, function(x){ csmfDraws_FRMT_LMM[[x]] })

# Combine predicted draws for HMM and LMM
csmfDraws_SAMP <- fn_formatDraws(csmfDraws_SAMP_HMM, csmfDraws_SAMP_LMM)

# Remove unnecessary objects
rm(csmfDraws_HMM, csmfDraws_LMM, csmfDraws_FRMT_HMM, csmfDraws_FRMT_LMM, envDraws)

## Squeezing

# Merge on envelopes
csmfDraws_envADD        <- mapply(function(x,y) fn_mergeEnv(x,y), csmfDraws_SAMP, envDraws_SAMP, SIMPLIFY = FALSE)
csmfList_envADD_GOODVR  <- lapply(envDraws_SAMP, function(x){ fn_mergeEnv(csmf_GOODVR, x)})
csmfList_envADD_CHN     <- lapply(envDraws_SAMP, function(x){ fn_mergeEnv(csmf_CHN, x)})

# Randomly assign CSMFs for VR/China for each draw
csmfDraws_GOODVR <- lapply(csmfList_envADD_GOODVR, function(x){ fn_randAssignVR(x, key_cod, CTRYGRP = "GOODVR")})
csmfDraws_CHN    <- lapply(csmfList_envADD_CHN, function(x){ fn_randAssignVR(x, key_cod, CTRYGRP = "CHN")})

# Prepare modeled countries and China for squeezing
csmfDraws_singlecauseADD <- lapply(csmfDraws_envADD, function(x) fn_prepareSqz(x, dat_tb, dat_hiv, dat_crisis, dat_meas, frac_cd, frac_lri))
csmfDraws_singlecauseADD_CHN <- lapply(csmfDraws_CHN, function(x) fn_prepareSqzChina(x, dat_hiv, dat_crisis, frac_cd))

# Randomly assign single causes for each draw
if(ageGroup == "05to09"){
  csmfDraws_singlecauseADD <- lapply(csmfDraws_singlecauseADD, function(x){ fn_randAssignMeas(x) })
}
csmfDraws_singlecauseADD <- lapply(csmfDraws_singlecauseADD, function(x){ fn_randAssignTB(x) })
csmfDraws_singlecauseADD <- lapply(csmfDraws_singlecauseADD, function(x){ fn_randAssignHIV(x) })
csmfDraws_singlecauseADD <- lapply(csmfDraws_singlecauseADD, function(x){ fn_randAssignCrisisEnd(x) })
csmfDraws_singlecauseADD_CHN <- lapply(csmfDraws_singlecauseADD_CHN, function(x){ fn_randAssignHIV(x) })

# Perform squeezing
if(ageGroup == "05to09"){
  csmfDraws_othercmpnSQZ     <- lapply(csmfDraws_singlecauseADD,     function(x){ fn_sqzOtherCMPN(x) })
  csmfDraws_lriSQZ           <- lapply(csmfDraws_othercmpnSQZ,       function(x){ fn_sqzLRI(x) })
  csmfDraws_crisisEndSQZ     <- lapply(csmfDraws_lriSQZ,             function(x){ fn_sqzCrisisEnd(x, key_cod, UNCERTAINTY = TRUE) })
  dthDraws_crisisEpiSQZ      <- lapply(csmfDraws_crisisEndSQZ,       function(x){ fn_sqzCrisisEpi(x, key_cod) })
  dthDraws_measEpiADD        <- lapply(dthDraws_crisisEpiSQZ,        function(x){ fn_addMeasEpi(x) })
  dthDraws_SQZ               <- dthDraws_measEpiADD
  csmfDraws_othercmpnSQZ_CHN <- lapply(csmfDraws_singlecauseADD_CHN, function(x){ fn_sqzOtherCMPNchina(x) })
  dthDraws_crisisEpiSQZ_CHN  <- lapply(csmfDraws_othercmpnSQZ_CHN,   function(x){ fn_sqzCrisisEpi(x, key_cod) })
  dthDraws_SQZ_CHN <- dthDraws_crisisEpiSQZ_CHN
}
if(ageGroup == "10to14"){
  csmfDraws_othercmpnSQZ     <- lapply(csmfDraws_singlecauseADD,     function(x){ fn_sqzOtherCMPN(x) })
  csmfDraws_lriSQZ           <- lapply(csmfDraws_othercmpnSQZ,       function(x){ fn_sqzLRI(x) })
  csmfDraws_crisisEndSQZ     <- lapply(csmfDraws_lriSQZ,             function(x){ fn_sqzCrisisEnd(x, key_cod, UNCERTAINTY = TRUE) })
  dthDraws_crisisEpiSQZ      <- lapply(csmfDraws_crisisEndSQZ,       function(x){ fn_sqzCrisisEpi(x, key_cod) })
  dthDraws_SQZ               <- dthDraws_crisisEpiSQZ
  csmfDraws_othercmpnSQZ_CHN <- lapply(csmfDraws_singlecauseADD_CHN, function(x){ fn_sqzOtherCMPNchina(x) })
  dthDraws_crisisEpiSQZ_CHN  <- lapply(csmfDraws_othercmpnSQZ_CHN,   function(x){ fn_sqzCrisisEpi(x, key_cod) })
  dthDraws_SQZ_CHN <- dthDraws_crisisEpiSQZ_CHN
}
if(ageGroup %in% c("15to19f", "15to19m")){
  csmfDraws_othercmpnSQZ     <- lapply(csmfDraws_singlecauseADD,     function(x){ fn_sqzOtherCMPN(x) })
  csmfDraws_crisisEndSQZ     <- lapply(csmfDraws_othercmpnSQZ,       function(x){ fn_sqzCrisisEnd(x, key_cod, UNCERTAINTY = TRUE) })
  dthDraws_crisisEpiSQZ      <- lapply(csmfDraws_crisisEndSQZ,       function(x){ fn_sqzCrisisEpi(x, key_cod) })
  dthDraws_SQZ               <- dthDraws_crisisEpiSQZ
  csmfDraws_othercmpnSQZ_CHN <- lapply(csmfDraws_singlecauseADD_CHN, function(x){ fn_sqzOtherCMPNchina(x) })
  dthDraws_crisisEpiSQZ_CHN  <- lapply(csmfDraws_othercmpnSQZ_CHN,   function(x){ fn_sqzCrisisEpi(x, key_cod) })
  dthDraws_SQZ_CHN <- dthDraws_crisisEpiSQZ_CHN
}
# Remove unnecessary objects
suppressWarnings(rm(csmfDraws_SAMP, envDraws_SAMP,
                    csmfDraws_othercmpnSQZ, csmfDraws_lriSQZ, csmfDraws_crisisEndSQZ, 
                    dthDraws_crisisEpiSQZ, dthDraws_measEpiADD, 
                    csmfDraws_othercmpnSQZ_CHN, dthDraws_crisisEpiSQZ_CHN))

# Format squeezed output
csmfDraws_SQZ <- mapply(function(x,y,z) fn_formatSqzOutput(x,y, z, key_cod), 
                        dthDraws_SQZ, dthDraws_SQZ_CHN, csmfDraws_envADD, SIMPLIFY = FALSE)

# Audit: check if squeezed CSMFs add up to 1 or contain NA
csmfDraws_SQZ_AUD  <- lapply(csmfDraws_SQZ, function(x){ fn_checkCSMFsqz(x, key_cod) })
names(csmfDraws_SQZ_AUD) <- 1:length(csmfDraws_SQZ_AUD)
df_csmfDraws_SQZ_AUD <- ldply(csmfDraws_SQZ_AUD, .id = "Draw")
if(nrow(df_csmfDraws_SQZ_AUD) > 0){
  write.csv(df_csmfDraws_SQZ_AUD, paste("./gen/uncertainty/audit/csmfDraws_SQZ_AUD_", ageGroup,".csv", sep=""), row.names = FALSE)
}

# Combine squeezed output from modeled countries (HMM and LMM) and China with GOODVR, format, save
csmfSqzDraws <- mapply(rbind, csmfDraws_SQZ, csmfDraws_GOODVR, SIMPLIFY = FALSE)
csmfSqzDraws <- lapply(csmfSqzDraws, function(x){ fn_formatAllOutput(x, key_cod) })
# Save temporary file for use in calculating aggregate age groups
saveRDS(csmfSqzDraws, file = paste("./gen/uncertainty/temp/csmfSqzDraws_", ageGroup, ".rds", sep=""))

# Calculate regional CSMFs
csmfSqzDraws_REG <- lapply(csmfSqzDraws, function(x){ fn_calcRegion(x, key_cod, key_region) })

# Remove unnecessary objects
rm(dthDraws_SQZ, dthDraws_SQZ_CHN, csmfDraws_envADD)

## Uncertainty

# Calculate uncertainty intervals
ui <- fn_calcUI(csmfSqzDraws, UI = 0.95, CODALL = codAll, ENV = env)
ui_REG <- fn_calcUI(csmfSqzDraws_REG, UI = 0.95, CODALL = codAll, REGIONAL = TRUE)

# Combine point estimates with uncertainty intervals
pointInt <- fn_combineUIpoint(ui, csmfSqz, codAll)
pointInt_REG <- fn_combineUIpoint(ui_REG, csmfSqz_REG, codAll, REGIONAL = TRUE)

# Round point estimates with uncertainty intervals
pointInt_FRMT <- fn_roundPointInt(pointInt, codAll)
pointInt_FRMT_REG <- fn_roundPointInt(pointInt_REG, codAll, REGIONAL = TRUE)

# Audit: check if point estimates fall in uncertainty bounds
pointInt_AUD <- fn_checkUI(pointInt_FRMT, codAll)
pointInt_AUD_REG <- fn_checkUI(pointInt_FRMT_REG, codAll, REGIONAL = TRUE)
if(nrow(pointInt_AUD) > 0){
  write.csv(pointInt_AUD, paste("./gen/uncertainty/audit/pointInt_AUD_", ageGroup,"_", resDate, ".csv", sep=""), row.names = FALSE)
}
if(nrow(pointInt_AUD_REG) > 0){
  write.csv(pointInt_AUD_REG, paste("./gen/uncertainty/audit/pointInt_AUD_", ageGroup,"REG_", resDate, ".csv", sep=""), row.names = FALSE)
}

# Adjust point estimates and uncertainty intervals
pointInt_ADJ <- fn_adjustPointInt(pointInt_FRMT, codAll)

# Audit: check if point estimates fall in uncertainty bounds
pointIntAdj_AUD <- fn_checkUI(pointInt_ADJ, codAll)
if(nrow(pointIntAdj_AUD) > 0){
  write.csv(pointIntAdj_AUD, paste("./gen/uncertainty/audit/pointIntAdj_AUD_", ageGroup,"_", resDate, ".csv", sep=""), row.names = FALSE)
}

# Save
write.csv(pointInt_ADJ, paste("./gen/uncertainty/output/pointInt_", ageGroup,".csv", sep=""), row.names = FALSE)
write.csv(pointInt_FRMT_REG, paste("./gen/uncertainty/output/pointInt_", ageGroup,"REG.csv", sep=""), row.names = FALSE)

# Clear environment
rm(list = ls())

################################################
# Results
################################################

# Load inputs and functions
source("./src/results/results_inputs.R")
source("./src/results/results_functions.R")

# Finalize estimates from squeezing pipeline (intermediate results)
csmfSqz_FRMT <- fn_roundCSMFsqz(csmfSqz, key_cod)
csmfSqz_PUB  <- fn_publishEstimates(csmfSqz_FRMT, key_region, key_ctryclass, codAll, UNCERTAINTY = FALSE)
write.csv(csmfSqz_PUB, paste("./gen/results/temp/PointEstimates_National_", ageGroup,"_", resDate, ".csv", sep=""), row.names = FALSE)

# Finalize estimates from uncertainty pipeline (final results)
point_PUB        <- fn_publishEstimates(pointInt, key_region, key_ctryclass, codAll, UNCERTAINTY = FALSE)
pointInt_PUB     <- fn_publishEstimates(pointInt, key_region, key_ctryclass, codAll, UNCERTAINTY = TRUE)
point_PUB_REG    <- fn_publishEstimates(pointInt_REG, key_region, key_ctryclass, codAll, UNCERTAINTY = FALSE, REGIONAL = TRUE)
pointInt_PUB_REG <- fn_publishEstimates(pointInt_REG, key_region, key_ctryclass, codAll, UNCERTAINTY = TRUE, REGIONAL = TRUE)

write.csv(point_PUB, paste("./gen/results/output/PointEstimates_National_", ageGroup,"_", resDate, ".csv", sep=""), row.names = FALSE)
write.csv(pointInt_PUB, paste("./gen/results/output/Uncertainty_National_", ageGroup,"_", resDate, ".csv", sep=""), row.names = FALSE)
write.csv(point_PUB_REG, paste("./gen/results/output/PointEstimates_Regional_", ageGroup,"_", resDate, ".csv", sep=""), row.names = FALSE)
write.csv(pointInt_PUB_REG, paste("./gen/results/output/Uncertainty_Regional_", ageGroup,"_", resDate, ".csv", sep=""), row.names = FALSE)

# Clear environment
rm(list = ls())

