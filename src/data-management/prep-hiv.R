################################################################################
#' @description Keep age-sex group of interest, fill in missing values with zero, re-scale to spectrum envelope
#' @return Data frame with c("ISO3", "Year", "Sex", "HIV", "hiv_lb", "hiv_ub")
################################################################################
#' Libraries
require(readstata13)
#' Inputs
source("./src/prepare-session/set-inputs.R")
source("./src/prepare-session/create-session-variables.R")
dth_hiv_u20_un <- read.csv('./data/single-causes/hiv/HIV2022Estimates_UNAIDS_11Nov2022.csv')
dth_hiv_u20_spec <- read.dta13("./data/single-causes/hiv/hiv_wppfractions_adol_5Jun2023.dta") 
key_ctryclass <- read.csv("./gen/data-prep/output/key_ctryclass_u20.csv")
env <- read.csv(paste("./gen/data-prep/output/env_",ageGroup,".csv", sep = ""))
################################################################################

# dth_hiv_u20_un <- read.csv() # need to get raw hiv data
# dth_hiv_u20_spec <- read.csv() # need to get raw hiv data
# Add code to create HIV2022Estimates_UNAIDS_11Nov2022.csv and hiv_wppfractions_adol_5Jun2023.dta from csv
# As a place holder, I will just do data prep on hiv_wppfractions_adol.dta
# and use the updated name for this object (dth_hiv_ageGroup)

## HIV data

dat1 <- dth_hiv_u20_un

dat1 <- dat1[, c('ISO3', 'E_Ind', 'Time', 'Value', 'lower.bound', 'upper.bound')]

dat1$age_lb <- NA
dat1$age_lb[dat1$E_Ind == 'M- AIDS deaths by age  5-9; Male+Female'] <- 5
dat1$age_lb[dat1$E_Ind == 'M- AIDS deaths by age  10-14 ; Male+Female'] <- 10
dat1$age_lb[dat1$E_Ind == 'M- AIDS deaths by age  15-19 ; Female'] <- 15
dat1$age_lb[dat1$E_Ind == 'M- AIDS deaths by age  15-19 ; Male'] <- 15

dat1$Sex <- sexLabels[1]
dat1$Sex[dat1$E_Ind == 'M- AIDS deaths by age  15-19 ; Female'] <- sexLabels[2]
dat1$Sex[dat1$E_Ind == 'M- AIDS deaths by age  15-19 ; Male'] <- sexLabels[3]

names(dat1)[names(dat1) == "Time"] <- "Year"
names(dat1)[names(dat1) == "Value"] <- "HIV"
names(dat1)[names(dat1) == "lower.bound"] <- "hiv_lb"
names(dat1)[names(dat1) == "upper.bound"] <- "hiv_ub"
dat1 <- dat1[, !names(dat1) == 'E_Ind']

# Keep age/sex group of interest
dat1 <- dat1[which(dat1$age_lb == ageLow & dat1$Sex %in% sexLabel), ]

## HIV data from spectrum

dat2 <- dth_hiv_u20_spec

names(dat2)[names(dat2) == "iso3"] <- "ISO3"
names(dat2)[names(dat2) == "year"] <- "Year"
names(dat2)[names(dat2) == "sex"] <- "Sex"

# Recode sex variable
dat2$Sex[dat2$Sex == "MF"] <- sexLabels[1]
dat2$Sex[dat2$Sex == "F"] <- sexLabels[2]
dat2$Sex[dat2$Sex == "M"] <- sexLabels[3]

# Keep age/sex group of interest
dat2 <- dat2[which(dat2$age_lb == ageLow & dat2$Sex %in% sexLabel), ]
dat2 <- dat2[,c(idVars, "dth_wpp")]

# Merge
dat <- merge(dat1, dat2, by = c("ISO3", "Year", "Sex"), all.x = T, all.y = F)

## Quality checks

# 1. Check that HIV deaths are inside lower and upper bounds
if(length(which(dat$HIV < dat$hiv_lb)) > 0){
  stop("HIV deaths outside of confidence bounds.")
}
if(length(which(dat$HIV > dat$hiv_ub)) > 0){
  stop("HIV deaths outside of confidence bounds.")
}

## Fill in missing values

# Create data frame for countries/years of interest
# For HIV data, HMM and LMM countries and China
df_ctryyears <- data.frame(ISO3 = rep(subset(key_ctryclass, Group2010 %in% c("HMM","LMM", "China DSP"))[,c("ISO3")], each = length(Years)),
                           Year = rep(Years),
                           Sex = sexLabel)

# Merge onto HIV data, identifying missing countries/years
dat <- merge(dat, df_ctryyears, by = idVars, all = TRUE)

# Recode missing HIV as 0
dat$HIV[which(is.na(dat$HIV))] <- 0

## Rescale dths to WPP envelope

# Merge on IGME deaths
dat <- merge(dat, env[,c("ISO3","Year","Deaths2")], by = c("ISO3", "Year"), all.x = TRUE)

# Rescale HIV deaths into IGME envelopes
# Note: I think originally this was done with Deaths1. 
# When we started using spectrum envelopes, seems it was changed to Deaths2?
# The function sqzSingle() had Deaths2
# The function sqzChina() had Deaths1
dat$HIV    <- ifelse(!is.na(dat$dth_wpp), dat$HIV * dat$Deaths2/dat$dth_wpp, dat$HIV)
dat$hiv_lb <- ifelse(!is.na(dat$dth_wpp), dat$hiv_lb * dat$Deaths2/dat$dth_wpp, dat$HIV)
dat$hiv_ub <- ifelse(!is.na(dat$dth_wpp), dat$hiv_ub * dat$Deaths2/dat$dth_wpp, dat$HIV)

# Tidy up
dat <- dat[, c("ISO3", "Year", "Sex", "HIV", "hiv_lb", "hiv_ub")]
rownames(dat) <- NULL

# Save output(s) ----------------------------------------------------------

write.csv(dat, paste("./gen/squeezing/input/dth_hiv_", ageGroup, ".csv", sep=""), row.names = FALSE)

# Remove unnecessary objects
rm(dth_hiv_u20_un, dth_hiv_u20_spec, key_ctryclass, env, dat1, dat2, df_ctryyears)

