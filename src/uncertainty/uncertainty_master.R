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
csmfDraws_SQZ <- mapply(function(w,x,y,z) fn_formatSqzOutput(w,x,y,z, key_cod), 
                        dthDraws_SQZ, dthDraws_SQZ_CHN, csmfDraws_envADD, csmfDraws_CHN, SIMPLIFY = FALSE)

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
csmfSqzDraws_REG <- lapply(csmfSqzDraws, function(x){ fn_calcRegion(x,  env_REG, codAll, key_region) })

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
if(nrow(pointInt_AUD) > 0){write.csv(pointInt_AUD, paste("./gen/uncertainty/audit/pointInt_AUD_", ageGroup,"_", resDate, ".csv", sep=""), row.names = FALSE)}
if(nrow(pointInt_AUD_REG) > 0){write.csv(pointInt_AUD_REG, paste("./gen/uncertainty/audit/pointInt_AUD_", ageGroup,"REG_", resDate, ".csv", sep=""), row.names = FALSE)}

# Adjust point estimates and uncertainty intervals
pointInt_ADJ <- fn_adjustPointIntZeroDeaths(pointInt_FRMT, codAll)
pointInt_ADJ <- fn_manuallyAdjustBounds(pointInt_ADJ)

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
