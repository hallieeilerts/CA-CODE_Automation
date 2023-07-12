################################################################################
#' @description Loads all libraries and inputs required for Squeezing.
#' @return Inputs loaded below
################################################################################
#' Libraries

#' Inputs
source("./src/prepare-session/set-inputs.R")
source("./src/prepare-session/create-session-variables.R")

# Classification keys
key_cod <- read.csv(paste("./gen/data-management/output/key_cod_", ageGroup, ".csv", sep=""))

# Envelope
env <- read.csv(paste("./gen/data-management/output/env_", ageGroup, ".csv", sep=""))

# Predicted CSMFs
csmf <- read.csv(paste("./gen/prediction/output/csmf_", ageGroup, ".csv", sep=""))

# Calculated CSMFs
csmf_CHN    <- read.csv(paste("./gen/prediction/output/csmf_",ageGroup, "CHN.csv", sep=""))
csmf_GOODVR <- read.csv(paste("./gen/prediction/output/csmf_", ageGroup, "GOODVR.csv", sep = ""))

# Single cause data
dth_tb     <- read.csv(paste("./gen/squeezing/input/dth_tb_", ageGroup, ".csv", sep=""))
dth_hiv    <- read.csv(paste("./gen/squeezing/input/dth_hiv_", ageGroup, ".csv", sep=""))
dth_crisis <- read.csv(paste("./gen/squeezing/input/dth_crisis_", ageGroup, ".csv", sep=""))
if(ageGroup == "05to09"){dth_meas <- read.csv(paste("./gen/squeezing/input/dth_meas_05to09.csv", sep=""))}
if(ageGroup %in% c("10to14","15to19f", "15to19m")){dth_meas <- NULL}

# Minimum fractions
minCD  <- readRDS(paste("./gen/squeezing/input/minfrac_cd_", ageGroup, ".rds", sep=""))
minLRI <- readRDS(paste("./gen/squeezing/input/minfrac_lri_", ageGroup, ".rds", sep=""))
################################################################################
