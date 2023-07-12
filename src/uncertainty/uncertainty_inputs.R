################################################################################
#' @description Loads all libraries and inputs required for Uncertainty
#' @return Inputs loaded below
################################################################################
#' Libraries
require(plyr)  # ldply()
require(msm)   # rtnorm()
#' Inputs
source("./src/prepare-session/set-inputs.R")
source("./src/prepare-session/create-session-variables.R")

## Prediction
# Classification keys
key_cod                <- read.csv(paste("./gen/data-management/output/key_cod_", ageGroup, ".csv", sep=""))
# Exposure data
db_pred_HMM            <- read.csv(paste("./gen/prediction/temp/db_pred_", ageGroup, "HMM.csv", sep=""))
db_pred_LMM            <- read.csv(paste("./gen/prediction/temp/db_pred_", ageGroup, "LMM.csv", sep=""))
# Outcome data
fit_HMM                <- readRDS(paste("./gen/estimation/output/mod_fit_", ageGroup, "HMM.rds", sep=""))
fit_LMM                <- readRDS(paste("./gen/estimation/output/mod_fit_", ageGroup, "LMM.rds", sep=""))
# For capping malaria fractions
dth_malaria_5to19      <-  read.csv("./gen/prediction/input/dth_malaria_5to19.csv")
csmf_malaria_01to04HMM <-  read.csv("./gen/prediction/input/csmf_malaria_01to04HMM.csv")

## Squeezing
# Envelope
env         <- read.csv(paste("./gen/data-management/output/env_", ageGroup, ".csv", sep=""))
# CSMFs
csmf        <- read.csv(paste("./gen/prediction/output/csmf_", ageGroup, ".csv",sep=""))
csmf_GOODVR <- read.csv(paste("./gen/prediction/output/csmf_", ageGroup, "GOODVR.csv", sep=""))
csmf_CHN    <- read.csv(paste("./gen/prediction/output/csmf_", ageGroup, "CHN.csv", sep=""))
# Single cause data
dth_tb      <- read.csv(paste("./gen/squeezing/input/dth_tb_", ageGroup, ".csv", sep=""))
dth_hiv     <- read.csv(paste("./gen/squeezing/input/dth_hiv_", ageGroup, ".csv", sep=""))
dth_crisis  <- read.csv(paste("./gen/squeezing/input/dth_crisis_", ageGroup, ".csv", sep=""))
if(ageGroup == "05to09"){dth_meas <- read.csv(paste("./gen/squeezing/input/dth_meas_05to09.csv", sep=""))}
if(ageGroup %in% c("10to14","15to19f", "15to19m")){dth_meas <- NULL}
# Minimum fractions
minCD      <- readRDS(paste("./gen/squeezing/input/minfrac_cd_", ageGroup, ".rds", sep=""))
minLRI     <- readRDS(paste("./gen/squeezing/input/minfrac_lri_", ageGroup, ".rds", sep=""))

## Uncertainty
# Envelope draws
draws_env  <- readRDS(paste("./gen/data-management/output/draws_env_", ageGroup, ".rds", sep=""))
################################################################################