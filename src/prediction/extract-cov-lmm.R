###################################################################
########################## BEGIN-INPUTS ###########################
###################################################################

# Load packages and session variables if not already loaded
if(!exists("sessionVars")){source("./src/prepare-session.R")
  load("./gen/data-prep/input/session-variables.Rdata")}

# Load input(s)
db_pred_u20 <- read.csv("./gen/data-prep/output/db_pred_u20.csv")
key_ctryclass_u20 <- read.csv("./gen/data-prep/output/key_ctryclass_u20.csv")
if(ageGroup == "05to09"){load("./gen/estimation/output/mod_fit_05to09LMM.RData")}
if(ageGroup == "10to14"){load("./gen/estimation/output/mod_fit_10to14LMM.RData")}
if(ageGroup == "15to19f"){load("./gen/estimation/output/mod_fit_15to19fLMM.RData")}
if(ageGroup == "15to19m"){load("./gen/estimation/output/mod_fit_15to19mLMM.RData")}

###################################################################
########################## END-INPUTS #############################
###################################################################

dat <- db_pred_u20
countryClass <- key_ctryclass_u20

# Covariates in model
vxf <- out$param$VXF
vxfAux <- vxf

# Identify sex-specific covariates and replace
vxfAux[paste(vxfAux, sexSuffix, sep = "_") %in% names(dat)] <- paste(vxfAux, sexSuffix, sep = "_")[paste(vxfAux, sexSuffix, sep = "_") %in% names(dat)]

# Identify smoothed covariate values and replace
vxfAux[paste(vxfAux, "sm", sep = "_") %in% names(dat)] <- paste(vxfAux, "sm", sep = "_")[paste(vxfAux, "sm", sep = "_") %in% names(dat)]

# Select covariates from original database
dat <- dat[, names(dat) %in% c("ISO3", "Year", vxfAux)]

# After selecting sex-specific and smoothed covariates, rename with names of covariates in model object
dat <- dat[, c("ISO3", "Year", vxfAux)]
names(dat)[-c(1:2)] <- vxf

# Select countries and update database
dat <- dat[dat$ISO3 %in% countryClass$ISO3[countryClass$Group2010 == "LMM"], ]

# Remove unnecessary objects
rm(db_pred_u20, key_ctryclass_u20, countryClass, vxf, vxfAux)

###################################################################
######################### BEGIN-OUTPUTS ###########################
###################################################################

# Save output(s)
write.csv(dat, paste("./gen/prediction/input/mod_covVal_", ageGroup, "LMM.csv",sep=""), row.names = FALSE)

###################################################################
######################### END-OUTPUTS #############################
###################################################################
