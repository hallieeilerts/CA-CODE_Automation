###################################################################
########################## BEGIN-INPUTS ###########################
###################################################################

# Load packages and session variables if not already loaded
if(!exists("sessionVars")){source("./src/prepare-session.R")
  load("./gen/data-prep/input/session-variables.Rdata")}

# Load input(s)
db_pred_u20 <- read.csv("./gen/data-prep/output/db_pred_u20.csv")
key_ctryclass_u20 <- read.csv("./gen/data-prep/output/key_ctryclass_u20.csv")
if(ageGroup == "05to09"){vxfHMM <- readRDS("./gen/estimation/temp/mod_covList_05to09HMM.rds")
                         vxfLMM <- readRDS("./gen/estimation/temp/mod_covList_05to09LMM.rds")}
if(ageGroup == "10to14"){vxfHMM <- readRDS("./gen/estimation/temp/mod_covList_10to14HMM.rds")
                         vxfLMM <- readRDS("./gen/estimation/temp/mod_covList_10to14LMM.rds")}
if(ageGroup == "15to19f"){vxfHMM <- readRDS("./gen/estimation/temp/mod_covList_15to19fHMM.rds")
                          vxfLMM <- readRDS("./gen/estimation/temp/mod_covList_15to19fLMM.rds")}
if(ageGroup == "15to19m"){vxfHMM <- readRDS("./gen/estimation/temp/mod_covList_15to19mHMM.rds")
                          vxfLMM <- readRDS("./gen/estimation/temp/mod_covList_15to19mLMM.rds")}

###################################################################
########################## END-INPUTS #############################
###################################################################

fn_extract_cov <- function(vxf, db_pred_u20, key_ctryclass_u20, ctryGrp){
  
  # Make another vector of covariates in model
  vxfAux <- vxf
  
  # Identify sex-specific covariates and replace
  vxfAux[paste(vxfAux, sexSuffix, sep = "_") %in% names(db_pred_u20)] <- paste(vxfAux, sexSuffix, sep = "_")[paste(vxfAux, sexSuffix, sep = "_") %in% names(db_pred_u20)]
  
  # Identify smoothed covariate values and replace
  vxfAux[paste(vxfAux, "sm", sep = "_") %in% names(db_pred_u20)] <- paste(vxfAux, "sm", sep = "_")[paste(vxfAux, "sm", sep = "_") %in% names(db_pred_u20)]
  
  # Select covariates from original database
  db_pred_u20 <- db_pred_u20[, names(db_pred_u20) %in% c("ISO3", "Year", vxfAux)]
  
  # After selecting sex-specific and smoothed covariates, rename with names of covariates in model object
  db_pred_u20 <- db_pred_u20[, c("ISO3", "Year", vxfAux)]
  names(db_pred_u20)[-c(1:2)] <- vxf
  
  # Select countries and update database
  db_pred_u20 <- db_pred_u20[db_pred_u20$ISO3 %in% key_ctryclass_u20$ISO3[key_ctryclass_u20$Group2010 == ctryGrp], ]
  
  return(db_pred_u20)
}

datHMM <- fn_extract_cov(vxfHMM, db_pred_u20, key_ctryclass_u20, "HMM")
datLMM <- fn_extract_cov(vxfLMM, db_pred_u20, key_ctryclass_u20, "LMM")

# Remove unnecessary objects
rm(vxf, db_pred_u20, key_ctryclass_u20)

###################################################################
######################### BEGIN-OUTPUTS ###########################
###################################################################

# Save output(s)
write.csv(datHMM, paste("./gen/prediction/input/mod_covVal_", ageGroup, "HMM.csv",sep=""), row.names = FALSE)
write.csv(datLMM, paste("./gen/prediction/input/mod_covVal_", ageGroup, "LMM.csv",sep=""), row.names = FALSE)
rm(datHMM, datLMM)

###################################################################
######################### END-OUTPUTS #############################
###################################################################
