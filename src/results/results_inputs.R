
###################################################################
########################## BEGIN-INPUTS ###########################
###################################################################

# Load packages and session variables if not already loaded
if(!exists("sessionVars")){source("./src/prepare-session.R")
  load("./gen/data-prep/input/session-variables.Rdata")}

# Results from prediction
csmf_Sqz <- read.csv(paste("./gen/squeezing/output/csmf_Sqz_", ageGroup, ".csv", sep = ""))
csmf_VR <- read.csv(paste("./gen/prediction/output/csmf_", ageGroup, "GOODVR.csv", sep = ""))
csmf_ALL <- rbind(csmf_Sqz, csmf_VR)

# Results from previous estimation round
if(ageGroup == "05to09"){csmf_OldResults <- read.csv("./data/previous-results/2000-2021/PointEstimates5to9-National.csv")
                         csmf_OldResults_REGION <- read.csv("./data/previous-results/2000-2021/PointEstimates5to9-Regional.csv")}
if(ageGroup == "10to14"){csmf_OldResults <- read.csv("./data/previous-results/2000-2021/PointEstimates10to14-National.csv")
                         csmf_OldResults_REGION <- read.csv("./data/previous-results/2000-2021/PointEstimates10to14-Regional.csv")}
if(ageGroup %in% c("15to19f", "15to19m")){csmf_OldResults <- read.csv("./data/previous-results/2000-2021/PointEstimates15to19-National.csv")
                                          csmf_OldResults_REGION <- read.csv("./data/previous-results/2000-2021/PointEstimates15to19-Regional.csv")}

# Classification keys
key_cod <- read.csv(paste("./gen/data-prep/output/key_cod_", ageGroup, ".csv", sep=""))
key_region <- read.csv("./gen/data-prep/output/key_region_u20.csv")
key_ctryclass <- read.csv("./gen/data-prep/output/key_ctryclass_u20.csv")

###################################################################
########################## END-INPUTS #############################
###################################################################