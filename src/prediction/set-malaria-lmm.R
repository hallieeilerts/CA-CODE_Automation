
###################################################################
########################## BEGIN-INPUTS ###########################
###################################################################

# Load packages and session variables if not already loaded
if(!exists("sessionVars")){source("./src/prepare-session.R")
  load("./gen/data-prep/input/session-variables.Rdata")}

# Load input(s)
if(ageGroup == "05to09"){dat <- read.csv("./gen/prediction/output/csmf_05to09LMM.csv")}
if(ageGroup == "10to14"){dat <- read.csv("./gen/prediction/output/csmf_10to14LMM.csv")}

###################################################################
########################## END-INPUTS #############################
###################################################################

dat$Malaria <- 0

###################################################################
######################### BEGIN-OUTPUTS ###########################
###################################################################

# Save output(s)
write.csv(dat, paste("./gen/prediction/output/csmf_SetMalaria_", ageGroup, "LMM.csv", sep=""), row.names = FALSE)

###################################################################
######################### END-OUTPUTS #############################
###################################################################
