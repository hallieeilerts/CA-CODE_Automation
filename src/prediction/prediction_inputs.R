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
db_pred <- read.csv("./gen/data-management/output/db_pred_u20.csv")

# Outcome data
db_VR <- read.csv("./gen/data-management/output/db_vr_5to19GOODVR.csv")
db_CHN <- read.csv("./gen/data-management/output/db_china_5to19.csv")

# Envelope
env <- read.csv(paste("./gen/data-management/output/env_", ageGroup, ".csv", sep=""))

# Model objects
vxf_HMM <- readRDS(paste("./gen/estimation/output/mod_covList_", ageGroup,"HMM.rds", sep=""))
vxf_LMM <- readRDS(paste("./gen/estimation/output/mod_covList_", ageGroup,"LMM.rds", sep=""))
fit_HMM <- readRDS(paste("./gen/estimation/output/mod_fit_", ageGroup, "HMM.rds", sep=""))
fit_LMM <- readRDS(paste("./gen/estimation/output/mod_fit_", ageGroup, "LMM.rds", sep=""))

# For capping malaria fractions
dth_malaria_5to19 <-  read.csv("./gen/prediction/input/dth_malaria_5to19.csv")
csmf_malaria_01to04HMM <-  read.csv("./gen/prediction/input/csmf_malaria_01to04HMM.csv")
################################################################################

