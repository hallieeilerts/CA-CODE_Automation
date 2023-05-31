
###################################################################
########################## BEGIN-INPUTS ###########################
###################################################################

# Load packages and session variables if not already loaded
if(!exists("sessionVars")){source("./src/prepare-session.R")
  load("./gen/data-prep/input/session-variables.Rdata")}

# Load input(s)
#if(ageGroup %in% c("00to28","01to04")){dat <- read.csv("./gen/data-prep/output/db_china_0to5.csv")}

if(ageGroup == "05to09"){dat <- read.csv("./gen/prediction/output/csmf_05to09CHN.csv")
                         datIGME <- read.csv("./gen/data-prep/output/env_05to09.csv")
                         dth_hiv <- read.csv("./gen/squeezing/input/dth_hiv_05to09.csv")
                         dth_crisis <- read.csv("./gen/squeezing/input/dth_crisis_05to09.csv")
                         load("./gen/squeezing/input/minfrac_cd_05to09.RData")}
if(ageGroup == "10to14"){dat <- read.csv("./gen/prediction/output/csmf_10to14CHN.csv")
                         datIGME <- read.csv("./gen/data-prep/output/env_10to14.csv")
                         dth_hiv <- read.csv("./gen/squeezing/input/dth_hiv_10to14.csv")
                         dth_crisis <- read.csv("./gen/squeezing/input/dth_crisis_10to14.csv")
                         load("./gen/squeezing/input/minfrac_cd_10to14.RData")}
if(ageGroup == "15to19f"){dat <- read.csv("./gen/prediction/output/csmf_15to19fCHN.csv")
                          datIGME <- read.csv("./gen/data-prep/output/env_15to19f.csv")
                          dth_hiv <- read.csv("./gen/squeezing/input/dth_hiv_15to19f.csv")
                          dth_crisis <- read.csv("./gen/squeezing/input/dth_crisis_15to19f.csv")
                          load("./gen/squeezing/input/minfrac_cd_15to19f.RData")}
if(ageGroup == "15to19m"){dat <- read.csv("./gen/prediction/output/csmf_15to19mCHN.csv")
                          datIGME <- read.csv("./gen/data-prep/output/env_15to19m.csv")
                          dth_hiv <- read.csv("./gen/squeezing/input/dth_hiv_15to19m.csv")
                          dth_crisis <- read.csv("./gen/squeezing/input/dth_crisis_15to19m.csv")
                          load("./gen/squeezing/input/minfrac_cd_15to19m.RData")}

###################################################################
########################## END-INPUTS #############################
###################################################################

# Merge on IGME envelopes
dat <- merge(dat, datIGME, by = idVars, all.x = T)
rm(datIGME)

# Merge on HIV
dat <- merge(dat, dth_hiv, by = idVars, all.x = T)
rm(dth_hiv)

# Merge on crisis
dat <- merge(dat, dth_crisis[, names(dth_crisis)[!names(dth_crisis) %in% c("CollectVio", "NatDis")]], 
                by = idVars, all.x = T)
rm(dth_crisis)

# Merge on minimum CD fraction and convert to deaths
dat$minCD <- dat$Deaths1 * minCD

#------------------------#
# 2023.02.23 PATCH
# Adjust epi deaths to envelopes: China
idEpi <- which(dat$epi_colvio + dat$epi_natdis > 0 &
                 dat$Deaths2 == dat$Deaths1)
if (length(idEpi) > 0) {
  dat$epi_colvio[idEpi] <- 0
  dat$epi_natdis[idEpi] <- 0
}
#------------------------#

# Remove unnecessary objects
rm(idEpi)

###################################################################
######################### BEGIN-OUTPUTS ###########################
###################################################################

# Save output(s)
write.csv(dat, paste("./gen/squeezing/temp/csmf_AddSinglecause_", ageGroup, "CHN.csv", sep=""), row.names = FALSE)

###################################################################
######################### END-OUTPUTS #############################
###################################################################

