################################################################################
#' @description Loads all libraries and inputs required for Squeezing.
#' @return Inputs loaded below
################################################################################
#' Libraries
library(plyr) # ddply
#' Inputs
source("./src/prepare-session/set-inputs.R")
source("./src/prepare-session/create-session-variables.R")

# Classification keys
key_cod <- read.csv(paste("./gen/data-management/output/key_cod_", ageGroup, ".csv", sep=""))
key_region <- read.csv("./gen/data-management/output/key_region_u20.csv")

# Envelope
env <- read.csv(paste("./gen/data-management/output/env_", ageGroup, ".csv", sep=""))

# Predicted CSMFs for modelled countries (HMM and LMM)
csmf <- read.csv(paste("./gen/prediction/output/csmf_", ageGroup, ".csv", sep=""))

# Calculated CSMFs
csmf_CHN    <- read.csv(paste("./gen/prediction/output/csmf_",ageGroup, "CHN.csv", sep=""))
csmf_GOODVR <- read.csv(paste("./gen/prediction/output/csmf_", ageGroup, "GOODVR.csv", sep = ""))

# Single cause data
dat_tb     <- read.csv(paste("./gen/squeezing/input/dat_tb_", ageGroup, ".csv", sep=""))
dat_hiv    <- read.csv(paste("./gen/squeezing/input/dat_hiv_", ageGroup, ".csv", sep=""))
dat_crisis <- read.csv(paste("./gen/squeezing/input/dat_crisis_", ageGroup, ".csv", sep=""))
if(ageGroup == "05to09"){dat_meas <- read.csv(paste("./gen/squeezing/input/dat_meas_05to09.csv", sep=""))}
if(ageGroup %in% c("10to14","15to19f", "15to19m")){dat_meas <- NULL}

# Minimum fractions
frac_cd  <- readRDS(paste("./gen/squeezing/input/frac_cd_", ageGroup, ".rds", sep=""))
frac_lri <- readRDS(paste("./gen/squeezing/input/frac_lri_", ageGroup, ".rds", sep=""))
################################################################################
