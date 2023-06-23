
###################################################################
########################## BEGIN-INPUTS ###########################
###################################################################

# Load packages and session variables if not already loaded
if(!exists("sessionVars")){source("./src/prepare-session.R")
  load("./gen/data-prep/input/session-variables.Rdata")}

# Load input(s)
# dth_hiv_5to19_wpp <- read.csv() # need to get raw hiv data
key_ctryclass <- read.csv("./gen/data-prep/output/key_ctryclass_u20.csv")
if(ageGroup == "05to09"){env <- read.csv("./gen/data-prep/output/env_05to09.csv")}
if(ageGroup == "10to14"){env <- read.csv("./gen/data-prep/output/env_10to14.csv")}
if(ageGroup == "15to19f"){env <- read.csv("./gen/data-prep/output/env_15to19f.csv")}
if(ageGroup == "15to19m"){env <- read.csv("./gen/data-prep/output/env_15to19m.csv")}

###################################################################
########################## END-INPUTS #############################
###################################################################

# Add code to create hiv_wppfractions_adol.dta from csv
# As a place holder, I will just do data prep on hiv_wppfractions_adol.dta
# and use the updated name for this object (dth_hiv_ageGroup)

## HIV data

dat <- read.csv('./data/single-causes/hiv/HIV2022Estimates_UNAIDS_11Nov2022.csv')
dat <- dat[, c('ISO3', 'E_Ind', 'Time', 'Value', 'lower.bound', 'upper.bound')]

dat$age_lb <- NA
dat$age_lb[dat$E_Ind == 'M- AIDS deaths by age  5-9; Male+Female'] <- 5
dat$age_lb[dat$E_Ind == 'M- AIDS deaths by age  10-14 ; Male+Female'] <- 10
dat$age_lb[dat$E_Ind == 'M- AIDS deaths by age  15-19 ; Female'] <- 15
dat$age_lb[dat$E_Ind == 'M- AIDS deaths by age  15-19 ; Male'] <- 15

dat$Sex <- sexLabels[1]
dat$Sex[dat$E_Ind == 'M- AIDS deaths by age  15-19 ; Female'] <- sexLabels[2]
dat$Sex[dat$E_Ind == 'M- AIDS deaths by age  15-19 ; Male'] <- sexLabels[3]

names(dat)[names(dat) == "Time"] <- "Year"
names(dat)[names(dat) == "Value"] <- "HIV"
names(dat)[names(dat) == "lower.bound"] <- "hiv_lb"
names(dat)[names(dat) == "upper.bound"] <- "hiv_ub"
dat <- dat[, !names(dat) == 'E_Ind']

# Keep age/sex group of interest
dat <- dat[which(dat$age_lb == ageLow & dat$Sex %in% sexLabel), ]

## Spectrum envelopes

dat_spec <- read.dta13("./data/single-causes/hiv/hiv_wppfractions_adol_5Jun2023.dta") 

names(dat_spec)[names(dat_spec) == "iso3"] <- "ISO3"
names(dat_spec)[names(dat_spec) == "year"] <- "Year"
names(dat_spec)[names(dat_spec) == "sex"] <- "Sex"

# Recode sex variable
dat_spec$Sex[dat_spec$Sex == "MF"] <- sexLabels[1]
dat_spec$Sex[dat_spec$Sex == "F"] <- sexLabels[2]
dat_spec$Sex[dat_spec$Sex == "M"] <- sexLabels[3]

# Keep age/sex group of interest
dat_spec <- dat_spec[which(dat_spec$age_lb == ageLow & dat_spec$Sex %in% sexLabel), ]

dat_spec <- dat_spec[,c(idVars, "dth_wpp")]

## Merge

dat <- merge(dat, dat_spec, by = c("ISO3", "Year", "Sex"), all.x = T, all.y = F)

#----------------#
#                #
# Quality checks #
#                #
#----------------#

# 1. Check that HIV deaths are inside lower and upper bounds
if(length(which(dat$HIV < dat$hiv_lb)) > 0){
  stop("HIV deaths outside of confidence bounds.")
}
if(length(which(dat$HIV > dat$hiv_ub)) > 0){
  stop("HIV deaths outside of confidence bounds.")
}


#------------------------#
#                        #
# Fill in missing values #
#                        #
#------------------------#

# Create data frame for countries/years of interest
# For HIV data, HMM and LMM countries and China
df_ctryyears <- data.frame(ISO3 = rep(subset(key_ctryclass, Group2010 %in% c("HMM","LMM", "China DSP"))[,c("ISO3")], each = length(Years)),
                           Year = rep(Years),
                           Sex = sexLabel)

# Merge onto HIV data, identifying missing countries/years
dat <- merge(dat, df_ctryyears, by = idVars, all = TRUE)

# Recode missing HIV as 0
dat$HIV[which(is.na(dat$HIV))] <- 0

#--------------------------#
#                          #
# Rescale dths to envelope #
#                          #
#--------------------------#

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

###################################################################
######################### BEGIN-OUTPUTS ###########################
###################################################################

# Save output(s)
write.csv(dat, paste("./gen/squeezing/input/dth_hiv_", ageGroup, ".csv", sep=""), row.names = FALSE)

# Remove unnecessary objects
rm(dat_spec, key_ctryclass, env, df_ctryyears)

###################################################################
######################### END-OUTPUTS #############################
###################################################################


