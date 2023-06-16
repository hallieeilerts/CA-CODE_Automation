###################################################################
########################## BEGIN-INPUTS ###########################
###################################################################

# Load packages and session variables if not already loaded
if(!exists("sessionVars")){source("./src/prepare-session.R")
  load("./gen/data-prep/input/session-variables.Rdata")}

# Load input(s)
if(ageGroup == "05to09"){load("./data/estimation/mod_fit/20210318-5to9-lam75SD007-VAMCM009-Test3.RData")}
if(ageGroup == "10to14"){load("./data/estimation/mod_fit/20201222-10to14-lam50SD007-VAMCM009-Test8j.RData")}
if(ageGroup == "15to19f"){load("./data/estimation/mod_fit/20210207-15to19Fem-lam50SD007-VAMCM009-Test9.RData")}
if(ageGroup == "15to19m"){load("./data/estimation/mod_fit/20210318-15to19Men-lam400SD007-VAMCM009-Test9e.RData")}


###################################################################
########################## END-INPUTS #############################
###################################################################

# Resave outputs with standardized name

###################################################################
######################### BEGIN-OUTPUTS ###########################
###################################################################

# Save output(s)
saveRDS(out, file = paste("./gen/estimation/output/mod_fit_", ageGroup, "HMM.rds",sep=""))

# Remove unnecessary objects
rm(out, fileName, model, rateTrans, refCat, sex, test, Time, vers)

###################################################################
######################### END-OUTPUTS #############################
###################################################################
