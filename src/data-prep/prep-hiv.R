
###################################################################
########################## BEGIN-INPUTS ###########################
###################################################################

# Load packages and session variables if not already loaded
if(!exists("sessionVars")){source("./src/prepare-session.R")
  load("./gen/data-prep/input/session-variables.Rdata")}

# Load input(s)
# dth_hiv_5to19_wpp <- read.csv() # need to get raw hiv data
key_ctryclass_u20 <- read.csv("./gen/data-prep/output/key_ctryclass_u20.csv")
if(ageGroup == "05to09"){datIGME <- read.csv("./gen/data-prep/output/env_05to09.csv")}
if(ageGroup == "10to14"){datIGME <- read.csv("./gen/data-prep/output/env_10to14.csv")}
if(ageGroup == "15to19f"){datIGME <- read.csv("./gen/data-prep/output/env_15to19f.csv")}
if(ageGroup == "15to19m"){datIGME <- read.csv("./gen/data-prep/output/env_15to19m.csv")}

###################################################################
########################## END-INPUTS #############################
###################################################################

# Add code to create hiv_wppfractions_adol.dta from csv
# As a place holder, I will just do data prep on hiv_wppfractions_adol.dta
# and use the updated name for this object (dth_hiv_ageGroup)

dat <- read.dta13("./data/single-causes/hiv/hiv_wppfractions_adol.dta") 
names(dat)[names(dat) == "iso3"] <- "ISO3"
names(dat)[names(dat) == "year"] <- "Year"
names(dat)[names(dat) == "sex"] <- "Sex"
names(dat)[names(dat) == "hiv_orig"] <- "HIV"

# Recode sex variable
dat$Sex[dat$Sex == "MF"] <- sexLabels[1]
dat$Sex[dat$Sex == "F"] <- sexLabels[2]
dat$Sex[dat$Sex == "M"] <- sexLabels[3]

# Keep age/sex group of interest
dat <- dat[which(dat$age_lb == ageLow & dat$Sex %in% sexLabel), ]

#------------------------#
#                        #
# Fill in missing values #
#                        #
#------------------------#

# Create data frame for countries/years of interest
# For HIV data, HMM and LMM countries and China
df_ctryyears <- data.frame(ISO3 = rep(subset(key_ctryclass_u20, Group2010 %in% c("HMM","LMM", "China DSP"))[,c("ISO3")], each = length(Years)),
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
dat <- merge(dat, datIGME[,c("ISO3","Year","Deaths1")], by = c("ISO3", "Year"), all.x = TRUE)

# Rescale HIV deaths into IGME envelopes
dat$HIV <- ifelse(!is.na(dat$dth_wpp), dat$HIV * dat$Deaths1/dat$dth_wpp, dat$HIV)

# Tidy up
dat <- dat[, c("ISO3", "Year", "Sex", "HIV")]
rownames(dat) <- NULL

###################################################################
######################### BEGIN-OUTPUTS ###########################
###################################################################

# Save output(s)
write.csv(dat, paste("./gen/squeezing/input/dth_hiv_", ageGroup, ".csv", sep=""), row.names = FALSE)

###################################################################
######################### END-OUTPUTS #############################
###################################################################


