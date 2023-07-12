################################################################################
#' @description Keep age-sex group of interest, fill in missing values with zero
#' @return Data frame with c("ISO3", "Year", "Sex", "epi_colvio", "epi_natdis", "CollectVio", "NatDis")
################################################################################
#' Libraries
require(readstata13)
#' Inputs
source("./src/prepare-session/set-inputs.R")
source("./src/prepare-session/create-session-variables.R")
dat           <- read.dta13("./data/single-causes/crisis/crisis_2000-2021_adolsplit_23Mar23.dta") 
env           <- read.csv(paste("./gen/data-management/output/env_",ageGroup,".csv", sep = ""))
key_ctryclass <- read.csv("./gen/data-management/output/key_ctryclass_u20.csv")
################################################################################

# Add code to create crisis_2000-2021_adolsplit_23Mar23.dta from csv
# As a place holder, I will just do data prep on crisis_2000-2021_adolsplit_23Mar23.dta
# and use the updated name for this object (dth_crisis_5to19)

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

# 1. Check that there are no endemic or epidemic crisis deaths for countries with zero deaths
# 2. Check that there are no epidemic crisis deaths when crisis-free and -included envelopes are same size
df_check <- dat
df_check$crisistotal <- apply(df_check[,!(names(df_check) %in% idVars)], 1, sum)
df_check$epicrisistotal <- df_check$epi_colvio + df_check$epi_natdis
df_check <- merge(df_check, env, by = idVars)
df_check$ind1 <- ifelse(df_check$crisistotal > 0 & df_check$Deaths2 == 0, 1, 0)
df_check$ind2 <- ifelse(df_check$epicrisistotal > 0 & df_check$Deaths1 == df_check$Deaths2, 1, 0)
if(sum(df_check$ind1) > 0){
  stop("Crisis deaths reported when country-year has zero deaths.")
}
if(sum(df_check$ind2) > 0){
  stop("Epidemic crisis deaths reported when crisis-free and crisis-included envelopes are the same.")
}

# Save output(s) ----------------------------------------------------------

write.csv(dat, paste("./gen/squeezing/input/dth_crisis_", ageGroup, ".csv", sep=""), row.names = FALSE)

# Remove unnecessary objects
rm(env, key_ctryclass, df_ctryyears, df_check)

