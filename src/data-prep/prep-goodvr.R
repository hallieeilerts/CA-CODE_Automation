###################################################################
########################## BEGIN-INPUTS ###########################
###################################################################

# Load packages and session variables if not already loaded
if(!exists("sessionVars")){source("./src/prepare-session.R")
  load("./gen/data-prep/input/session-variables.Rdata")}

# Load input(s)
# db_vr_5to19_who <- read.csv() # need to get raw WHO data

###################################################################
########################## END-INPUTS #############################
###################################################################

# Add code to create 20210331-GoodVR-5to19.dta
# Presumably this was down by cleaning the raw WHO data, only keeping countries of interest.

# As a place holder, I will just convert 20210331-GoodVR-5to19.dta into a csv
# and use the updated name for this object (db_vr_5to19GOODV)
dat <- read.dta13("./gen/data-prep/temp/20210331-GoodVR-5to19.dta", nonint.factors = T)

# Rename variables
names(dat)[names(dat) == "iso3"] <- "ISO3"
names(dat)[names(dat) == "year"] <- "Year"

# Create new age and sex variables
names(dat)[names(dat) == "age_lb"] <- "AgeLow"
names(dat)[names(dat) == "sex"] <- "Sex"
dat$Sex[dat$Sex == 3] <- sexLabels[1]
dat$Sex[dat$Sex == 1] <- sexLabels[3]
dat$Sex[dat$Sex == 2] <- sexLabels[2]

# Re-label variables
# GOODVR
names(dat)[names(dat) == "neo"] <- "neoplasm"

# Remove China if it is in this data
dat <- subset(dat, ISO3 != "CHN")

# Delete unnecessary columns
dat <- dat[, !names(dat) %in% c("post_source", "post_source2",  "igmedeaths", "total", "check")]

###################################################################
######################### BEGIN-OUTPUTS ###########################
###################################################################

# Save output(s)
write.csv(dat, "./gen/data-prep/output/db_vr_5to19GOODVR.csv", row.names = FALSE)

###################################################################
######################### END-OUTPUTS #############################
###################################################################
