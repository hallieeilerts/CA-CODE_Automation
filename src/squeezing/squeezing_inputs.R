
###################################################################
########################## BEGIN-INPUTS ###########################
###################################################################

# Load packages and session variables if not already loaded
if(!exists("sessionVars")){source("./src/prepare-session.R")
  load("./gen/data-prep/input/session-variables.Rdata")}

# Predicted CSMF
csmf <- read.csv(paste("./gen/prediction/output/csmf_", ageGroup, ".csv", sep=""))

# Calculated CSMF
csmf_CHN <- read.csv(paste("./gen/prediction/output/csmf_",ageGroup, "CHN.csv", sep=""))
csmf_GOODVR <- read.csv(paste("./gen/prediction/output/csmf_", ageGroup, "GOODVR.csv", sep = ""))

# Envelope
env <- read.csv(paste("./gen/data-prep/output/env_", ageGroup, ".csv", sep=""))

# Classification keys
key_cod <- read.csv(paste("./gen/data-prep/output/key_cod_", ageGroup, ".csv", sep=""))

# Single cause data
dth_tb <- read.csv(paste("./gen/squeezing/input/dth_tb_", ageGroup, ".csv", sep=""))
dth_hiv <- read.csv(paste("./gen/squeezing/input/dth_hiv_", ageGroup, ".csv", sep=""))
dth_crisis <- read.csv(paste("./gen/squeezing/input/dth_crisis_", ageGroup, ".csv", sep=""))
if(ageGroup == "05to09"){dth_meas <- read.csv(paste("./gen/squeezing/input/dth_meas_05to09.csv", sep=""))}
if(ageGroup %in% c("10to14","15to19f", "15to19m")){dth_meas <- NULL}

# Minimum fractions
minCD <- readRDS(paste("./gen/squeezing/input/minfrac_cd_", ageGroup, ".rds", sep=""))
minLRI <- readRDS(paste("./gen/squeezing/input/minfrac_lri_", ageGroup, ".rds", sep=""))

###################################################################
########################## END-INPUTS #############################
###################################################################