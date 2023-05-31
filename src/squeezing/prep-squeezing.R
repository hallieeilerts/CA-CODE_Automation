
###################################################################
########################## BEGIN-INPUTS ###########################
###################################################################

# Load packages and session variables if not already loaded
if(!exists("sessionVars")){source("./src/prepare-session.R")
  load("./gen/data-prep/input/session-variables.Rdata")}

# Load input(s)
if(ageGroup == "05to09"){datHMM <- read.csv("./gen/prediction/output/csmf_CapMalaria_05to09HMM.csv")
                         datLMM <- read.csv("./gen/prediction/output/csmf_SetMalaria_05to09LMM.csv")
                         datIGME <- read.csv("./gen/data-prep/output/env_05to09.csv")
                         dth_tb <- read.csv("./gen/squeezing/input/dth_tb_05to09.csv")
                         dth_hiv <- read.csv("./gen/squeezing/input/dth_hiv_05to09.csv")
                         dth_crisis <- read.csv("./gen/squeezing/input/dth_crisis_05to09.csv")
                         dth_meas <- read.csv("./gen/squeezing/input/dth_meas_05to09.csv")
                         load("./gen/squeezing/input/minfrac_lri_05to09.RData")
                         load("./gen/squeezing/input/minfrac_cd_05to09.RData")}
if(ageGroup == "10to14"){datHMM <- read.csv("./gen/prediction/output/csmf_CapMalaria_10to14HMM.csv")
                         datLMM <- read.csv("./gen/prediction/output/csmf_SetMalaria_10to14LMM.csv")
                         datIGME <- read.csv("./gen/data-prep/output/env_10to14.csv")
                         dth_tb <- read.csv("./gen/squeezing/input/dth_tb_10to14.csv")
                         dth_hiv <- read.csv("./gen/squeezing/input/dth_hiv_10to14.csv")
                         dth_crisis <- read.csv("./gen/squeezing/input/dth_crisis_10to14.csv")
                         load("./gen/squeezing/input/minfrac_lri_10to14.RData")
                         load("./gen/squeezing/input/minfrac_cd_10to14.RData")}
if(ageGroup == "15to19f"){datHMM <- read.csv("./gen/prediction/output/csmf_15to19fHMM.csv")
                          datLMM <- read.csv("./gen/prediction/output/csmf_15to19fLMM.csv")
                          datIGME <- read.csv("./gen/data-prep/output/env_15to19f.csv")
                          dth_tb <- read.csv("./gen/squeezing/input/dth_tb_15to19f.csv")
                          dth_hiv <- read.csv("./gen/squeezing/input/dth_hiv_15to19f.csv")
                          dth_crisis <- read.csv("./gen/squeezing/input/dth_crisis_15to19f.csv")
                          load("./gen/squeezing/input/minfrac_cd_15to19f.RData")}
if(ageGroup == "15to19m"){datHMM <- read.csv("./gen/prediction/output/csmf_15to19mHMM.csv")
                          datLMM <- read.csv("./gen/prediction/output/csmf_15to19mLMM.csv")
                          datIGME <- read.csv("./gen/data-prep/output/env_15to19m.csv")
                          dth_tb <- read.csv("./gen/squeezing/input/dth_tb_15to19m.csv")
                          dth_hiv <- read.csv("./gen/squeezing/input/dth_hiv_15to19m.csv")
                          dth_crisis <- read.csv("./gen/squeezing/input/dth_crisis_15to19m.csv")
                          load("./gen/squeezing/input/minfrac_cd_15to19m.RData")}

###################################################################
########################## END-INPUTS #############################
###################################################################

# Merge HMM and LMM
dat <- rbind(datHMM, datLMM)
rm(datHMM, datLMM)

# Merge on IGME envelopes
dat <- merge(dat, datIGME, by = idVars, all.x = T)
rm(datIGME)

# Merge on TB
dat <- merge(dat, dth_tb, by = idVars, all.x = T)
rm(dth_tb)

# Merge on HIV
dat <- merge(dat, dth_hiv, by = idVars, all.x = T)
rm(dth_hiv)

# Merge on measles
if(ageGroup == "05to09"){dat <- merge(dat, dth_meas, by = idVars, all.x = T)
rm(dth_meas)
}

# Merge on crisis
dat <- merge(dat, dth_crisis, by = idVars, all.x = T)
rm(dth_crisis)

# Merge on minimum CD fraction and convert to deaths
dat$minCD <- dat$Deaths1 * minCD

# Merge on minimum LRI fraction and convert to deaths
if(ageGroup %in% c("05to09", "10to14")){dat$minLRI <- dat$Deaths1 * minLRI}

#------------------------#
# 2023.02.23 PATCH
# Adjust epi deaths to envelopes: All countries
v_idEpi <- which(dat$epi_colvio + dat$epi_natdis > 0 & dat$Deaths2 == dat$Deaths1)
if (length(v_idEpi) > 0) {
  dat$epi_colvio[v_idEpi] <- 0
  dat$epi_natdis[v_idEpi] <- 0
}
#------------------------#

# Exclude country-years with no deaths
datNODEATHS <- dat[which(dat$Deaths1 == 0), ]
dat <- dat[which(dat$Deaths1 > 0), ]
rownames(dat) <- NULL

# Remove unnecessary objects
rm(v_idEpi)

# Note: the modeled causes are fractions and the single-causes are death counts

###################################################################
######################### BEGIN-OUTPUTS ###########################
###################################################################

# Save output(s)
write.csv(dat, paste("./gen/squeezing/temp/csmf_AddSinglecause_", ageGroup, ".csv", sep=""), row.names = FALSE)
write.csv(datNODEATHS, paste("./gen/squeezing/temp/csmf_", ageGroup, "NODEATHS.csv", sep=""), row.names = FALSE)
rm(datNODEATHS)

###################################################################
######################### END-OUTPUTS #############################
###################################################################
