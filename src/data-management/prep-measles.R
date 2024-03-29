################################################################################
#' @description Rename columns for endemic, epidemic, and total measles; keep age-sex group of interest; merge deaths with uncertainty; fill in missing values with zero
#' @return Data frame with c("ISO3", "Year", "Sex", "Measles", "meas_epi", "msl", "msl_lb", "msl_ub")
################################################################################
#' Libraries
require(readstata13)
#' Inputs
source("./src/prepare-session/set-inputs.R")
source("./src/prepare-session/create-session-variables.R")
dat_meas_5to19_WHO <- read.dta13("./data/single-causes/measles/measles_2000-2021_adol.dta")
ui_meas_5to19_WHO  <- read.dta13("./data/single-causes/measles/measles_2000-2021_adolunc.dta") 
key_ctryclass_u20  <- read.csv("./gen/data-management/output/key_ctryclass_u20.csv")
################################################################################

# dth_meas_5to19_who <- read.csv() # need to get raw measles data
# unc_meas_5to19_who <- read.csv() # need to get raw measles data
# Add code to create measles_2000-2021_adol.dta
# As a place holder, I will just do data prep on measles_2000-2021_adol.dta
# and use the updated name for this object (dth_meas_ageGroup)

# Measles deaths
dat1 <- dat_meas_5to19_WHO

names(dat1)[names(dat1) == "iso3"] <- idVars[1]
names(dat1)[names(dat1) == "year"] <- idVars[2]
names(dat1)[names(dat1) == "sex"] <- idVars[3]
dat1$msl <- dat1$measin + dat1$measout
names(dat1)[names(dat1) == "measin"] <- "Measles"
names(dat1)[names(dat1) == "measout"] <- "meas_epi"

# Recode sex variable
dat1$Sex[dat1$Sex == "MF"] <- sexLabels[1]
dat1$Sex[dat1$Sex == "F"] <- sexLabels[2]
dat1$Sex[dat1$Sex == "M"] <- sexLabels[3]

# Keep age/sex group of interest
dat1 <- dat1[which(dat1$age_lb == ageLow & dat1$age_ub == ageUp & dat1$Sex %in% sexLabel), ]

# Measles uncertainty
dat2 <- ui_meas_5to19_WHO

names(dat2)[names(dat2) == "iso3"] <- idVars[1]
names(dat2)[names(dat2) == "year"] <- idVars[2]
# Delete total measles column
# Will use the one from dat1, calculated as measin + measout
dat2$msl <- NULL

dat <- merge(dat1, dat2, by = c("ISO3", "Year"), all.x = TRUE)

# Check that all expected countries are included --------------------------

if(sum(!(unique(key_ctryclass_u20$ISO3) %in% dat$ISO3)) > 0){
  warning("Not all countries included in data input.")
  write.table(sort(unique(key_ctryclass_u20$WHOname)[!(unique(key_ctryclass_u20$ISO3) %in% dat$ISO3)]), 
              "./gen/data-management/audit/missing_meas.txt")
}

# Fill in zeros for missing country-years, if necessary

# Create data frame for countries/years of interest
# For measles data, HMM and LMM countries
df_ctryyears <- data.frame(ISO3 = rep(key_ctryclass_u20$ISO3, each = length(Years)),
                           Year = rep(Years),
                           Sex = sexLabel)

# Merge onto measles data, identifying missing countries/years
dat <- merge(dat, df_ctryyears, by = idVars, all = TRUE)

# Recode missing measles as 0
dat$Measles[which(is.na(dat$Measles))] <- 0
dat$meas_epi[which(is.na(dat$meas_epi))] <- 0
dat$msl[which(is.na(dat$msl))] <- 0
dat$msl_lb[which(is.na(dat$msl_lb))] <- 0
dat$msl_ub[which(is.na(dat$msl_ub))] <- 0

# Tidy up
dat <- dat[, c("ISO3", "Year", "Sex", "Measles", "meas_epi", "msl", "msl_lb", "msl_ub")]
dat <- dat[order(dat$ISO3, dat$Year),]
rownames(dat) <- NULL

# Save output(s) ----------------------------------------------------------

dat_meas <- dat

write.csv(dat, paste("./gen/squeezing/input/dat_meas_", ageGroup, ".csv", sep=""), row.names = FALSE)
