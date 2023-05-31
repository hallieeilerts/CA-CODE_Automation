###################################################################
########################## BEGIN-INPUTS ###########################
###################################################################

# Load packages and session variables if not already loaded
if(!exists("sessionVars")){source("./src/prepare-session.R")
  load("./gen/data-prep/input/session-variables.Rdata")}

# Load input(s)
#if(ageGroup == "05to09"){load("./gen/estimation/input/mod_input_05to09HMM.RData")}
#if(ageGroup == "10to14"){load("./gen/estimation/input/mod_input_10to14HMM.RData")}
#if(ageGroup == "15to19f"){load("./gen/estimation/input/mod_input_15to19fHMM.RData")}
#if(ageGroup == "15to19m"){load("./gen/estimation/input/mod_input_15to19mHMM.RData")}

###################################################################
########################## END-INPUTS #############################
###################################################################

# PLACEHOLDER
# Add code is used to select covariates.
# As a placeholder, I extract the covarites from the output.
if(ageGroup == "05to09"){load("./gen/estimation/output/mod_fit_05to09HMM.RData")}
if(ageGroup == "10to14"){load("./gen/estimation/output/mod_fit_10to14HMM.RData")}
if(ageGroup == "15to19f"){load("./gen/estimation/output/mod_fit_15to19fHMM.RData")}
if(ageGroup == "15to19m"){load("./gen/estimation/output/mod_fit_15to19mHMM.RData")}
vxf <- out$param$VXF

###################################################################
######################### BEGIN-OUTPUTS ###########################
###################################################################

# Save output(s)
saveRDS(vxf, file = paste("./gen/estimation/temp/mod_covList_", ageGroup, "HMM.rds",sep=""))

###################################################################
######################### END-OUTPUTS #############################
###################################################################