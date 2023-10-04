################################################################################
#' @description Loads all libraries and inputs required for Uncertainty
#' @return Inputs loaded below
################################################################################
#' Libraries
require(plyr)  # ldply()
require(msm)   # rtnorm()
require(data.table) # melt(), dcast()
#' Inputs
source("./src/prepare-session/set-inputs.R")
source("./src/prepare-session/create-session-variables.R")

## Prediction sub-section inputs
# Exposure data
dat_pred_HMM           <- read.csv(paste("./gen/prediction/temp/dat_pred_", ageGroup, "HMM.csv", sep=""))
dat_pred_LMM           <- read.csv(paste("./gen/prediction/temp/dat_pred_", ageGroup, "LMM.csv", sep=""))
# Outcome data
mod_fit_HMM            <- readRDS(paste("./gen/estimation/output/mod_fit_", ageGroup, "HMM.rds", sep=""))
mod_fit_LMM            <- readRDS(paste("./gen/estimation/output/mod_fit_", ageGroup, "LMM.rds", sep=""))
# For capping malaria fractions
dat_malaria_5to19      <-  read.csv("./gen/prediction/input/dat_malaria_5to19.csv")
frac_malaria_01to04HMM <-  read.csv("./gen/prediction/input/frac_malaria_01to04.csv")

## Squeezing sub-section inputs
# Classification keys
key_cod     <- read.csv(paste("./gen/data-management/output/key_cod_", ageGroup, ".csv", sep=""))
key_region  <- read.csv("./gen/data-management/output/key_region_u20.csv")
# Envelopes
env         <- read.csv(paste("./gen/data-management/output/env_", ageGroup, ".csv", sep=""))
env_REG     <- read.csv(paste("./gen/data-management/output/env_", ageGroup, "REG.csv", sep=""))
# CSMFs
csmf_GOODVR <- read.csv(paste("./gen/prediction/output/csmf_", ageGroup, "GOODVR.csv", sep=""))
csmf_CHN    <- read.csv(paste("./gen/prediction/output/csmf_", ageGroup, "CHN.csv", sep=""))
# Single cause data
dat_tb      <- read.csv(paste("./gen/squeezing/input/dat_tb_", ageGroup, ".csv", sep=""))
dat_hiv     <- read.csv(paste("./gen/squeezing/input/dat_hiv_", ageGroup, ".csv", sep=""))
dat_crisis  <- read.csv(paste("./gen/squeezing/input/dat_crisis_", ageGroup, ".csv", sep=""))
if(ageGroup == "05to09"){dat_meas <- read.csv(paste("./gen/squeezing/input/dat_meas_05to09.csv", sep=""))}
if(ageGroup %in% c("10to14","15to19f", "15to19m")){dat_meas <- NULL}
# Minimum fractions
frac_cd      <- readRDS(paste("./gen/squeezing/input/frac_cd_", ageGroup, ".rds", sep=""))
frac_lri     <- readRDS(paste("./gen/squeezing/input/frac_lri_", ageGroup, ".rds", sep=""))

## Uncertainty sub-section inputs
# CSMFs that have been processed in squeezing pipeline (contains all countries, even those not subject to squeezing)
csmfSqz     <- read.csv(paste("./gen/squeezing/output/csmfSqz_", ageGroup, ".csv", sep = ""))
csmfSqz_REG <- read.csv(paste("./gen/squeezing/output/csmfSqz_", ageGroup, "REG.csv", sep = ""))
# Envelope draws
envDraws  <- readRDS(paste("./gen/data-management/output/envDraws_", ageGroup, ".rds", sep=""))
################################################################################