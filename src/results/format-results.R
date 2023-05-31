
###################################################################
########################## BEGIN-INPUTS ###########################
###################################################################

# Load packages and session variables if not already loaded
if(!exists("sessionVars")){source("./src/prepare-session.R")
  load("./gen/data-prep/input/session-variables.Rdata")}

# Load input(s)
if(ageGroup == "05to09"){dat <- read.csv("./gen/squeezing/output/csmf_Sqz_05to09.csv")
                         datVR <- read.csv("./gen/prediction/output/csmf_05to09GOODVR.csv")}
if(ageGroup == "10to14"){dat <- read.csv("./gen/squeezing/output/csmf_Sqz_10to14.csv")
                         datVR <- read.csv("./gen/prediction/output/csmf_10to14GOODVR.csv")}
if(ageGroup == "15to19f"){dat <- read.csv("./gen/squeezing/output/csmf_Sqz_15to19f.csv")
                          datVR <- read.csv("./gen/prediction/output/csmf_15to19fGOODVR.csv")}
if(ageGroup == "15to19m"){dat <- read.csv("./gen/squeezing/output/csmf_Sqz_15to19m.csv")
                          datVR <- read.csv("./gen/prediction/output/csmf_15to19mGOODVR.csv")}

###################################################################
########################## END-INPUTS #############################
###################################################################

dat <- rbind(dat, datVR)

# Round deaths
dat$Deaths <- round(dat$Deaths)
# Round rate
dat$Rate <- round(dat$Rate, 5)
# Round fractions
dat[,!(names(dat) %in% c(idVars, "Deaths", "Rate"))] <- round(dat[,!(names(dat) %in% c(idVars, "Deaths", "Rate"))], 5)

# Tidy up
dat <- dat[order(dat$ISO3, dat$Year, dat$Sex), ]
rownames(dat) <- NULL

# Remove unnecessary objects
rm(datVR)

###################################################################
######################### BEGIN-OUTPUTS ###########################
###################################################################

# Save output(s)
write.csv(dat, paste("./gen/results/output/PointEstimates_National_", ageGroup,"_", format(Sys.Date(), format="%Y%m%d"),".csv", sep=""), row.names = FALSE)

###################################################################
######################### END-OUTPUTS #############################
###################################################################