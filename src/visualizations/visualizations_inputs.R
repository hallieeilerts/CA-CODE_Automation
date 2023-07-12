
###################################################################
########################## BEGIN-INPUTS ###########################
###################################################################

# Load packages and session variables if not already loaded
if(!exists("sessionVars")){source("./src/prepare-session.R")
  load("./gen/data-prep/input/session-variables.Rdata")}

# CSMFs: point estimates
csmf <- read.csv(paste("./gen/results/output/PointEstimates_National_", ageGroup,"_", resDate, ".csv", sep=""))
csmf_REGIONAL <- read.csv(paste("./gen/results/output/PointEstimates_Regional_", ageGroup,"_", resDate, ".csv", sep=""))

### Load formatted versions from results
# CSMFs: uncertainty intervals
#unc_csmf <- read.csv(paste("./gen/uncertainty/output/unc_csmf_", ageGroup, ".csv", sep = ""))

# Point estimates from previous estimation round
if(ageGroup == "05to09"){csmf_OldResults <- read.csv("./data/previous-results/PointEstimates_2000-2019_National_05to09.csv")
                         csmf_OldResults_REGIONAL <- read.csv("./data/previous-results/PointEstimates_2000-2019_Regional_05to09.csv")}
if(ageGroup == "10to14"){csmf_OldResults <- read.csv("./data/previous-results/PointEstimates_2000-2019_National_10to14.csv")
                         csmf_OldResults_REGIONAL <- read.csv("./data/previous-results/PointEstimates_2000-2019_Regional_10to14.csv")}
if(ageGroup %in% c("15to19f", "15to19m")){csmf_OldResults <- read.csv("./data/previous-results/PointEstimates_2000-2019_National_15to19.csv")
                                          csmf_OldResults_REGIONAL <- read.csv("./data/previous-results/PointEstimates_2000-2019_Regional_15to19.csv")}

# Classification keys
key_cod <- read.csv(paste("./gen/data-prep/output/key_cod_", ageGroup, ".csv", sep=""))
key_region <- read.csv("./gen/data-prep/output/key_region_u20.csv")
key_ctryclass <- read.csv("./gen/data-prep/output/key_ctryclass_u20.csv")

###################################################################
########################## END-INPUTS #############################
###################################################################