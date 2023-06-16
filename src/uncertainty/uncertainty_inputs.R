
###################################################################
########################## BEGIN-INPUTS ###########################
###################################################################

# Load packages and session variables if not already loaded
if(!exists("sessionVars")){source("./src/prepare-session.R")
  load("./gen/data-prep/input/session-variables.Rdata")}

# ------------------From gen/estimation and /prediction ------------------------#
# Outcome data
fit_HMM <- readRDS(paste("./gen/estimation/output/mod_fit_", ageGroup, "HMM.rds", sep=""))
fit_LMM <- readRDS(paste("./gen/estimation/output/mod_fit_", ageGroup, "LMM.rds", sep=""))

# For capping malaria fractions
dth_malaria_5to19 <-  read.csv("./gen/prediction/input/dth_malaria_5to19.csv")
csmf_malaria_01to04HMM <-  read.csv("./gen/prediction/input/csmf_malaria_01to04HMM.csv")

# Extracted covariate values from prediction database
db_pred_HMM <- read.csv(paste("./gen/prediction/temp/db_pred_", ageGroup, "HMM.csv", sep=""))
db_pred_LMM <- read.csv(paste("./gen/prediction/temp/db_pred_", ageGroup, "LMM.csv", sep=""))

# Predicted csmfs
csmf <- read.csv(paste("./gen/prediction/output/csmf_", ageGroup, ".csv",sep=""))
# ------------------------------------------------------------------------------#

draws_env <- readRDS(paste("./gen/data-prep/output/draws_env", ageGroup, ".rds", sep=""))

###################################################################
########################## END-INPUTS #############################
###################################################################