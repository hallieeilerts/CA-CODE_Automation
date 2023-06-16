
###################################################################
########################## BEGIN-INPUTS ###########################
###################################################################

# Load packages and session variables if not already loaded
if(!exists("sessionVars")){source("./src/prepare-session.R")
  load("./gen/data-prep/input/session-variables.Rdata")}

# Outcome data
db_VR <- read.csv("./gen/data-prep/output/db_vr_5to19GOODVR.csv")
db_CHN <- read.csv("./gen/data-prep/output/db_china_5to19.csv")
fit_HMM <- readRDS(paste("./gen/estimation/output/mod_fit_", ageGroup, "HMM.rds", sep=""))
fit_LMM <- readRDS(paste("./gen/estimation/output/mod_fit_", ageGroup, "LMM.rds", sep=""))

# Envelope
env <- read.csv(paste("./gen/data-prep/output/env_", ageGroup, ".csv", sep=""))

# Classification keys
key_cod <- read.csv(paste("./gen/data-prep/output/key_cod_", ageGroup, ".csv", sep=""))
key_ctryclass <- read.csv("./gen/data-prep/output/key_ctryclass_u20.csv")

# Covariates used in model
vxf_HMM <- readRDS(paste("./gen/estimation/temp/mod_covList_", ageGroup,"HMM.rds", sep=""))
vxf_LMM <- readRDS(paste("./gen/estimation/temp/mod_covList_", ageGroup,"LMM.rds", sep=""))

# Prediction database
db_pred <- read.csv("./gen/data-prep/output/db_pred_u20.csv")

# For capping malaria fractions
dth_malaria_5to19 <-  read.csv("./gen/prediction/input/dth_malaria_5to19.csv")
csmf_malaria_01to04HMM <-  read.csv("./gen/prediction/input/csmf_malaria_01to04HMM.csv")

###################################################################
########################## END-INPUTS #############################
###################################################################
