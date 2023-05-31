
###################################################################
########################## BEGIN-INPUTS ###########################
###################################################################

# Load packages and session variables if not already loaded
if(!exists("sessionVars")){source("./src/prepare-session.R")
  load("./gen/data-prep/input/session-variables.Rdata")}

# Load input(s)
csmf_Sqz_01to04HMM <- read.dta13("./data/mortality-fractions/child_cod_2000-2021.dta")

###################################################################
########################## END-INPUTS #############################
###################################################################

dat <- csmf_Sqz_01to04HMM
dat <- dat[dat$level == "country", ]
dat$pct_malaria_1to59 <- dat$post8/rowSums(dat[, grep("post", names(dat))], na.rm = T)
dat <- dat[, c(1, 2, ncol(dat))]
names(dat)[names(dat) == "iso3"] <- "ISO3"
names(dat)[names(dat) == "year"] <- "Year"
rownames(dat) <- NULL

# Name main variable
names(dat)[names(dat) == "pct_malaria_1to59"] <- "csmf_malaria_01to04"

# Remove unnecessary objects
rm(csmf_Sqz_01to04HMM)

###################################################################
######################### BEGIN-OUTPUTS ###########################
###################################################################

# Save output(s)
write.csv(dat, paste("./gen/prediction/input/csmf_malaria_01to04HMM.csv", sep=""), row.names = FALSE)

###################################################################
######################### END-OUTPUTS #############################
###################################################################
