###################################################################
########################## BEGIN-INPUTS ###########################
###################################################################

# Load packages and session variables if not already loaded
if(!exists("sessionVars")){source("./src/prepare-session.R")
  load("./gen/data-prep/input/session-variables.Rdata")}

# Load input(s)
if(ageGroup == "05to09"){fitHMM <- readRDS("./gen/estimation/output/mod_fit_05to09HMM.rds")
                         fitLMM <- readRDS("./gen/estimation/output/mod_fit_05to09LMM.rds")
                         # load("./gen/estimation/output/mod_fit_05to09HMM.RData")
                         covHMM <- read.csv("./gen/prediction/input/mod_covVal_05to09HMM.csv")
                         covLMM <- read.csv("./gen/prediction/input/mod_covVal_05to09LMM.csv")}
if(ageGroup == "10to14"){fitHMM <- readRDS("./gen/estimation/output/mod_fit_10to14HMM.rds")
                         fitLMM <- readRDS("./gen/estimation/output/mod_fit_10to14LMM.rds")
                         # load("./gen/estimation/output/mod_fit_10to14HMM.RData")
                         covHMM <- read.csv("./gen/prediction/input/mod_covVal_10to14HMM.csv")
                         covLMM <- read.csv("./gen/prediction/input/mod_covVal_10to14LMM.csv")}
if(ageGroup == "15to19f"){fitHMM <- readRDS("./gen/estimation/output/mod_fit_15to19fHMM.rds")
                          fitLMM <- readRDS("./gen/estimation/output/mod_fit_15to19fLMM.rds")
                          # load("./gen/estimation/output/mod_fit_15to19fHMM.RData")
                          covHMM <- read.csv("./gen/prediction/input/mod_covVal_15to19fHMM.csv")
                          covLMM <- read.csv("./gen/prediction/input/mod_covVal_15to19fLMM.csv")}
if(ageGroup == "15to19m"){fitHMM <- readRDS("./gen/estimation/output/mod_fit_15to19mHMM.rds")
                          fitLMM <- readRDS("./gen/estimation/output/mod_fit_15to19mLMM.rds")
                          # load("./gen/estimation/output/mod_fit_15to19mHMM.RData")
                          covHMM <- read.csv("./gen/prediction/input/mod_covVal_15to19mHMM.csv")
                          covLMM <- read.csv("./gen/prediction/input/mod_covVal_15to19mLMM.csv")}

###################################################################
########################## END-INPUTS #############################
###################################################################

# Run prediction function for each year
l_res <- lapply(Years, function(x){fn_call_p1New(x, fitHMM, covHMM)})
datHMM <- do.call(rbind, l_res)
l_res <- lapply(Years, function(x){fn_call_p1New(x, fitLMM, covLMM)})
datLMM <- do.call(rbind, l_res)

# Format predicted fractions
fn_format_prediction <- function(dat){
  
  # Add column for sex
  dat$Sex <- sexLabel
  # Add empty "Maternal" column for 15-19 males
  if(sexLabel == sexLabels[3]){dat$Maternal <- 0}
  
  # Rename predicted CODs
  # Note: Use these names in model estimation next round so there's no need to adjust.
  names(dat)[names(dat) == "OtherCD"] <- "OtherCMPN"
  names(dat)[names(dat) == "RTA"] <- "RTI"
  names(dat)[names(dat) == "Self_harm"] <- "SelfHarm"
  names(dat)[names(dat) == "Interp_violence"] <- "InterpVio"
  names(dat)[names(dat) == "Other_inj"] <- "OtherInj"
  
  # Tidy up
  dat <- dat[, c(idVars, sort(names(dat)[which(!names(dat) %in% idVars)]))]
  dat <- dat[order(dat$ISO3, dat$Year),]
}

datHMM <- fn_format_prediction(datHMM)
datLMM <- fn_format_prediction(datLMM)

# Remove unnecessary objects
rm(fitHMM, fitLMM, covHMM, covLMM, l_res)

###################################################################
######################### BEGIN-OUTPUTS ###########################
###################################################################

# Save output(s)
write.csv(datHMM, paste("./gen/prediction/output/csmf_", ageGroup, "HMM.csv",sep=""), row.names = FALSE)
write.csv(datLMM, paste("./gen/prediction/output/csmf_", ageGroup, "LMM.csv",sep=""), row.names = FALSE)
rm(datHMM, datLMM)

###################################################################
######################### END-OUTPUTS #############################
###################################################################

