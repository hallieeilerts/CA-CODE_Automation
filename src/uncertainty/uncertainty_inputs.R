
###################################################################
########################## BEGIN-INPUTS ###########################
###################################################################

# Load packages and session variables if not already loaded
if(!exists("sessionVars")){source("./src/prepare-session.R")
  load("./gen/data-prep/input/session-variables.Rdata")}

# -------------------------Prediction inputs------------------------------------#
# Outcome data
fit_HMM <- readRDS(paste("./gen/estimation/output/mod_fit_", ageGroup, "HMM.rds", sep=""))
fit_LMM <- readRDS(paste("./gen/estimation/output/mod_fit_", ageGroup, "LMM.rds", sep=""))

# For capping malaria fractions
dth_malaria_5to19 <-  read.csv("./gen/prediction/input/dth_malaria_5to19.csv")
csmf_malaria_01to04HMM <-  read.csv("./gen/prediction/input/csmf_malaria_01to04HMM.csv")
# ------------------------------------------------------------------------------#

# Extracted covariate values from prediction database
db_pred_HMM <- read.csv(paste("./gen/prediction/temp/db_pred_", ageGroup, "HMM.csv", sep=""))
db_pred_LMM <- read.csv(paste("./gen/prediction/temp/db_pred_", ageGroup, "LMM.csv", sep=""))

# Predicted csmfs
csmf <- read.csv(paste("./gen/prediction/output/csmf_", ageGroup, ".csv",sep=""))

# IGME draws
if(ageGroup == "05to09"){fileDeaths <- '5-9/death0.ctj.rda'
                         fileRates <- '5-9/imr.ctj.rda'}
if(ageGroup == "10to14"){fileDeaths <- '10-14/death1to4.ctj.rda'
                         fileRates <- '10-14/cmr.ctj.rda'}
if(ageGroup == "15to19f"){fileDeaths <- '15-19/female/death0.ctj.rda'
                          fileRates <- '15-19/female/imr.ctj.rda'}
if(ageGroup == "15to19m"){fileDeaths <- '15-19/male/death0.ctj.rda'
                          fileRates <- '15-19/male/imr.ctj.rda'}

# Classification keys
key_region <- read.csv("./gen/data-prep/output/key_region_u20.csv")
key_ctryclass <- read.csv("./gen/data-prep/output/key_ctryclass_u20.csv")
key_regclass <- merge(key_region, key_ctryclass[, c("ISO3", "Group2010", "FragileState")])


###################################################################
########################## END-INPUTS #############################
###################################################################