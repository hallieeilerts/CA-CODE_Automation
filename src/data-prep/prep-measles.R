
###################################################################
########################## BEGIN-INPUTS ###########################
###################################################################

# Load packages and session variables if not already loaded
if(!exists("sessionVars")){source("./src/prepare-session.R")
  load("./gen/data-prep/input/session-variables.Rdata")}

# Load input(s)
# dth_meas_5to19_who <- read.csv() # need to get raw measles data
key_ctryclass <- read.csv("./gen/data-prep/output/key_ctryclass_u20.csv")

###################################################################
########################## END-INPUTS #############################
###################################################################

# Add code to create measles_2000-2021_adol.dta

# As a place holder, I will just do data prep on measles_2000-2021_adol.dta
# and use the updated name for this object (dth_meas_ageGroup)

dat <- read.dta13("./data/single-causes/measles/measles_2000-2021_adol.dta") 
names(dat)[names(dat) == "iso3"] <- idVars[1]
names(dat)[names(dat) == "year"] <- idVars[2]
names(dat)[names(dat) == "sex"] <- idVars[3]
dat$msl <- dat$measin + dat$measout
names(dat)[names(dat) == "measin"] <- "Measles"
names(dat)[names(dat) == "measout"] <- "epi_meas"

# Recode sex variable
dat$Sex[dat$Sex == "MF"] <- sexLabels[1]
dat$Sex[dat$Sex == "F"] <- sexLabels[2]
dat$Sex[dat$Sex == "M"] <- sexLabels[3]

# Keep age/sex group of interest
dat <- dat[which(dat$age_lb == ageLow & dat$age_ub == ageUp & dat$Sex %in% sexLabel), ]

#------------------------#
#                        #
# Fill in missing values #
#                        #
#------------------------#

# Create data frame for countries/years of interest
# For measles data, HMM and LMM countries
df_ctryyears <- data.frame(ISO3 = rep(subset(key_ctryclass, Group2010 %in% c("HMM","LMM"))[,c("ISO3")], each = length(Years)),
                           Year = rep(Years),
                           Sex = sexLabel)

# Merge onto measles data, identifying missing countries/years
dat <- merge(dat, df_ctryyears, by = idVars, all = TRUE)

# Recode missing measles as 0
dat$Measles[which(is.na(dat$Measles))] <- 0
dat$epi_meas[which(is.na(dat$epi_meas))] <- 0

# Tidy up
dat <- dat[, c("ISO3", "Year", "Sex", "Measles", "epi_meas")]
rownames(dat) <- NULL

# Remove unnecessary objects
rm(key_ctryclass, df_ctryyears)

###################################################################
######################### BEGIN-OUTPUTS ###########################
###################################################################

# Save output(s)
write.csv(dat, paste("./gen/squeezing/input/dth_meas_", ageGroup, ".csv", sep=""), row.names = FALSE)

###################################################################
######################### END-OUTPUTS #############################
###################################################################
