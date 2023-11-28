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
  l_csmf_HMM <- lapply(l_csmf_HMM, function(x){fn_capMalFrac(x, dat_malaria_05to19, frac_malaria_01to04)})
  l_csmf_LMM <- lapply(l_csmf_LMM, function(x){fn_setMalFrac(x)})
}

# Format predicted CSMFs, save
csmf <- fn_formatPrediction(l_csmf_HMM, l_csmf_LMM)
write.csv(csmf, paste("./gen/prediction/output/csmf_", ageGroup, ".csv",sep=""), row.names = FALSE)

# Clear environment
rm(list = ls())