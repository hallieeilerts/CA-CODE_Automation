################################################################################
#' @description Loads all libraries and inputs required for Prediction.
#' @return Inputs loaded below
################################################################################
#' Libraries

#' Inputs
source("./src/prepare-session/set-inputs.R")
source("./src/prepare-session/create-session-variables.R")

# Classification keys
key_cod <- read.csv(paste("./gen/data-management/output/key_cod_", ageGroup, ".csv", sep=""))
key_ctryclass <- read.csv("./gen/data-management/output/key_ctryclass_u20.csv")

# Exposure data
dat_pred <- read.csv("./gen/data-management/output/dat_pred_u20.csv")

# Outcome data
dat_VR  <- read.csv("./gen/data-management/output/dat_vr_5to19GOODVR.csv")
dat_CHN <- read.csv("./gen/data-management/output/dat_srs_5to19CHN.csv")

# Envelope
env <- read.csv(paste("./gen/data-management/output/env_", ageGroup, ".csv", sep=""))

# Model objects
mod_covNames_HMM <- readRDS(paste("./gen/estimation/output/mod_covNames_", ageGroup,"HMM.rds", sep=""))
mod_covNames_LMM <- readRDS(paste("./gen/estimation/output/mod_covNames_", ageGroup,"LMM.rds", sep=""))
mod_fit_HMM <- readRDS(paste("./gen/estimation/output/mod_fit_", ageGroup, "HMM.rds", sep=""))
mod_fit_LMM <- readRDS(paste("./gen/estimation/output/mod_fit_", ageGroup, "LMM.rds", sep=""))

# For capping malaria fractions
dat_malaria_5to19   <-  read.csv("./gen/prediction/input/dat_malaria_5to19.csv")
frac_malaria_01to04 <-  read.csv("./gen/prediction/input/frac_malaria_01to04.csv")
################################################################################

