################################################################################
#' @description Keep age-sex group of interest, fill in missing values with zero
#' @return Data frame with c("ISO3", "Year", "Sex", "epi_colvio", "epi_natdis", "CollectVio", "NatDis")
################################################################################
#' Libraries
require(readstata13)
#' Inputs
source("./src/prepare-session/set-inputs.R")
source("./src/prepare-session/create-session-variables.R")
dat_crisis_5to19_IGME <- read.dta13("./data/single-causes/crisis/crisis_2000-2021_adolsplit_16Jun23_squeezed.dta") 
env                   <- read.csv(paste("./gen/data-management/output/env_", ageGroup,".csv", sep = ""))
key_ctryclass         <- read.csv("./gen/data-management/output/key_ctryclass_u20.csv")
################################################################################

# Add code to create crisis_2000-2021_adolsplit_23Mar23.dta from csv
# As a place holder, I will just do data prep on crisis_2000-2021_adolsplit_23Mar23.dta
# and use the updated name for this object (dth_crisis_5to19)
dat <- dat_crisis_5to19_IGME

names(dat)[names(dat) == "iso3"] <- "ISO3"
names(dat)[names(dat) == "year"] <- "Year"
names(dat)[names(dat) == "sex"] <- "Sex"
names(dat)[names(dat) == "end_colvio"] <- "CollectVio"
names(dat)[names(dat) == "end_natdis"] <- "NatDis"

# Recode sex variable
dat$Sex[dat$Sex == "MF"] <- sexLabels[1]
dat$Sex[dat$Sex == "F"] <- sexLabels[2]
dat$Sex[dat$Sex == "M"] <- sexLabels[3]

# Keep age/sex group of interest
dat <- dat[which(dat$age_lb == ageLow & dat$Sex %in% sexLabel), ]

## Fill in missing values

# Create data frame for countries/years of interest
# For crisis data, HMM and LMM countries and China
df_ctryyears <- data.frame(ISO3 = rep(subset(key_ctryclass, Group2010 %in% c("HMM","LMM", "China DSP"))[,c("ISO3")], each = length(Years)),
                           Year = rep(Years),
                           Sex = sexLabel)

# Merge onto crisis data, identifying missing countries/years
dat <- merge(dat, df_ctryyears, by = idVars, all = TRUE)

# Recode missing values as 0
dat$CollectVio[which(is.na(dat$CollectVio))] <- 0
dat$NatDis[which(is.na(dat$NatDis))] <- 0
dat$epi_colvio[which(is.na(dat$epi_colvio))] <- 0
dat$epi_natdis[which(is.na(dat$epi_natdis))] <- 0

# Tidy up
dat <- dat[, c("ISO3","Year","Sex","epi_colvio", "epi_natdis", "CollectVio", "NatDis")]

# Quality checks ----------------------------------------------------------

#------------------#
# PATCH 2023.02.23  from sqzAndMerge() in Functions001

# Check 1
# No negative epidemic crisis
dat$epi_colvio[which(dat$epi_colvio < 0)] <- 0
dat$epi_natdis[which(dat$epi_natdis < 0)] <- 0

# Check 2
# Adjust epi deaths to envelopes: All countries
# There should be no epidemic crisis deaths when crisis-free and -included envelopes are same size
idEpi <- which(dat$epi_colvio + dat$epi_natdis > 0 & dat$Deaths1 == dat$Deaths2)
if(length(idEpi) > 0){
  warning("Epidemic crisis deaths reported when crisis-free and crisis-included envelopes are the same.")
  dat$epi_colvio[idEpi] <- 0
  dat$epi_natdis[idEpi] <- 0
}

# Check 3
# There should be no no endemic or epidemic crisis deaths for countries with zero deaths
idEpi <- which(dat$epi_colvio + dat$epi_natdis + dat$CollectVio + dat$NatDis > 0 & dat$Deaths2 == 0)
if(length(idEpi) > 0){
  warning("Endemic or epidemic crisis deaths reported when country-year has zero deaths.")
  dat$epi_colvio[idEpi] <- 0
  dat$epi_natdis[idEpi] <- 0
  dat$CollectVio[idEpi] <- 0
  dat$NatDis[idEpi] <- 0
}

# Save output(s) ----------------------------------------------------------

dat_crisis <- dat

write.csv(dat_crisis, paste("./gen/squeezing/input/dat_crisis_", ageGroup, ".csv", sep=""), row.names = FALSE)
