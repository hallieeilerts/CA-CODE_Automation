################################################################################
#' @description Updates variable names of China DSP data
#' @return Data frame with China CSMFs with updated variable names.
################################################################################
#' Libraries
#' Inputs
source("./src/prepare-session/set-inputs.R")
source("./src/prepare-session/create-session-variables.R")
dat_srs_5to19CHN_DSP <- read.dta13("./data/china/20210330-ChinaDSP.dta", nonint.factors = T)
################################################################################

dat <- dat_srs_5to19CHN_DSP

# Add code to create 20210330-ChinaDSP.dta
# db_china_5to19_dsp <- read.csv() # need to get raw China data
# As a place holder, I will just convert 20210330-ChinaDSP.dta into a csv
# and use the updated name for this object (db_china_5to19)

# Add country variable
dat$ISO3 <- "CHN"

# Rename variables
names(dat)[names(dat) == "year"] <- "Year"

# Create new age and sex variables
dat$AgeLow <- 5
dat$AgeLow[dat$group == "Both 10-14"] <- 10
dat$AgeLow[dat$group == "Female 15-19_(4)"] <- 15
dat$AgeLow[dat$group == "Male 15-19"] <- 15
dat$Sex <- sexLabels[1]
dat$Sex[dat$group == "Female 15-19_(4)"] <- sexLabels[2]
dat$Sex[dat$group == "Male 15-19"] <- sexLabels[3]

# Re-label variables
names(dat)[names(dat) == "csdf3"] <- "dia"
names(dat)[names(dat) == "csdf4"] <- "mea"
names(dat)[names(dat) == "csdf7"] <- "mening"
names(dat)[names(dat) == "csdf9"] <- "lri"
names(dat)[names(dat) == "csdf10"] <- "tb"
names(dat)[names(dat) == "csdf11"] <- "maternal"
names(dat)[names(dat) == "csdf12"] <- "othercd"
names(dat)[names(dat) == "csdf14"] <- "congen"
names(dat)[names(dat) == "csdf15"] <- "neoplasm"
names(dat)[names(dat) == "csdf16"] <- "cardio"
names(dat)[names(dat) == "csdf17"] <- "endo"
names(dat)[names(dat) == "csdf18"] <- "digest"
names(dat)[names(dat) == "csdf19"] <- "otherncd"
names(dat)[names(dat) == "csdf21"] <- "rta"
names(dat)[names(dat) == "csdf22"] <- "drown"
names(dat)[names(dat) == "csdf23"] <- "natdis"
names(dat)[names(dat) == "csdf24"] <- "intvio"
names(dat)[names(dat) == "csdf25"] <- "colvio"
names(dat)[names(dat) == "csdf27"] <- "selfharm"
names(dat)[names(dat) == "csdf28"] <- "otherinj"

# Save output(s) ----------------------------------------------------------

dat_srs_5to19CHN <- dat

write.csv(dat_srs_5to19CHN, paste("./gen/data-management/output/dat_srs_5to19CHN.csv", sep=""), row.names = FALSE)
