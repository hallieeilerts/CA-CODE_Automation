
###################################################################
########################## BEGIN-INPUTS ###########################
###################################################################

# Load packages and session variables if not already loaded
if(!exists("sessionVars")){source("./src/prepare-session.R")
  load("./gen/data-prep/input/session-variables.Rdata")}

# Load input(s)
csmf_Sqz <- read.csv(paste("./gen/squeezing/output/csmf_Sqz_", ageGroup, ".csv", sep = ""))
csmfVR <- read.csv(paste("./gen/prediction/output/csmf_", ageGroup, "GOODVR.csv", sep = ""))
key_region <- read.csv("./gen/data-prep/output/key_region_u20.csv")
key_ctryclass <- read.csv("./gen/data-prep/output/key_ctryclass_u20.csv")

###################################################################
########################## END-INPUTS #############################
###################################################################

# Combine csmfs for modelled countries with VR
dat <- rbind(csmf_Sqz, csmfVR)

# Run formatting function
dat <- fn_format_results(dat, key_region, key_ctryclass, codAll)

# Remove unnecessary objects
rm(csmf_Sqz, csmfVR, key_cod, key_region, key_ctryclass)

###################################################################
######################### BEGIN-OUTPUTS ###########################
###################################################################

# Save output(s)
write.csv(dat, paste("./gen/results/output/PointEstimates_National_", ageGroup,"_", format(Sys.Date(), format="%Y%m%d"),".csv", sep=""), row.names = FALSE)

###################################################################
######################### END-OUTPUTS #############################
###################################################################