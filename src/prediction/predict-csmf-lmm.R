###################################################################
########################## BEGIN-INPUTS ###########################
###################################################################

# Load packages and session variables if not already loaded
if(!exists("sessionVars")){source("./src/prepare-session.R")
  load("./gen/data-prep/input/session-variables.Rdata")}

# Load input(s)
if(ageGroup == "05to09"){load("./gen/estimation/output/mod_fit_05to09LMM.RData")
                        dat <- read.csv("./gen/prediction/input/mod_covVal_05to09LMM.csv")}
if(ageGroup == "10to14"){load("./gen/estimation/output/mod_fit_10to14LMM.RData")
                         dat <- read.csv("./gen/prediction/input/mod_covVal_10to14LMM.csv")}
if(ageGroup == "15to19f"){load("./gen/estimation/output/mod_fit_15to19fLMM.RData")
                          dat <- read.csv("./gen/prediction/input/mod_covVal_15to19fLMM.csv")}
if(ageGroup == "15to19m"){load("./gen/estimation/output/mod_fit_15to19mLMM.RData")
                          dat <- read.csv("./gen/prediction/input/mod_covVal_15to19mLMM.csv")}

###################################################################
########################## END-INPUTS #############################
###################################################################

mod_fit <- out
mod_covVal <- dat

#------------------------------------#
# PREDICTED FRACTIONS FROM THE MODEL #
#------------------------------------#

l_dat <- lapply(Years, function(x){fn_call_p1New(x, mod_fit, mod_covVal)})
dat <- do.call(rbind, l_dat)

# Tidy up
dat <- dat[, c(idVars, sort(names(dat)[which(!names(dat) %in% idVars)]))]
dat <- dat[order(dat$ISO3, dat$Year),]

#---------------------------------------------------------------#
# Delete this next round when correct name is used in model_fit #
names(dat)[names(dat) == "OtherCD"] <- "OtherCMPN"
names(dat)[names(dat) == "RTA"] <- "RTI"
names(dat)[names(dat) == "Self_harm"] <- "SelfHarm"
names(dat)[names(dat) == "Interp_violence"] <- "InterpVio"
names(dat)[names(dat) == "Other_inj"] <- "OtherInj"
#---------------------------------------------------------------#

# Add empty "Maternal" column for 15-19 males
if(ageGroup == "15to19m"){dat$Maternal <- 0}

# Remove unnecessary objects
rm(l_dat, mod_fit, mod_covVal, out)

# Checking to make sure results the same
# old <- read.csv("./gen/prediction/output/_old/csmf_15to19mLMM.csv")
# old <- melt(setDT(old), id.vars = idVars)
# names(old)[names(old) == "OtherCD"] <- "OtherCMPN"
# new <- dat
# new <- melt(setDT(new), id.vars = idVars)
# test <- merge(old, new, by =c("ISO3","Year", "Sex", "variable"), suffixes = c("old","new"))
# test$dif <- test$valueold - test$valuenew
# max(test$dif)


###################################################################
######################### BEGIN-OUTPUTS ###########################
###################################################################

# Save output(s)
write.csv(dat, paste("./gen/prediction/output/csmf_", ageGroup, "LMM.csv",sep=""), row.names = FALSE)

###################################################################
######################### END-OUTPUTS #############################
###################################################################
