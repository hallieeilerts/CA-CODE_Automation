################################################################################
#' @description Rename columns
#' @return Data frame with c("ISO3", "Year", "dth_malaria_5to19")
################################################################################
#' Libraries
require(readstata13)
#' Inputs
dat_malaria_u20_MAP <- read.dta13("./data/single-causes/malaria/malaria_adol_2Nov2022.dta")
################################################################################

dat <- dat_malaria_u20_MAP

# Add code to create malaria_adol_2Nov2022.dta
# As a place holder, I will just convert malaria_adol_2Nov2022.dta into a csv
# and use the updated name for this object (dth_malaria_5to19)

names(dat)[names(dat) == "iso"] <- "ISO3"
names(dat)[names(dat) == "year"] <- "Year"
names(dat)[names(dat) == "malcases"] <- "dth_malaria_5to19"

# Select variables of interest
dat <- dat[, c("ISO3", "Year", "dth_malaria_5to19")]
rownames(dat) <- NULL

###################################################################
######################### BEGIN-OUTPUTS ###########################
###################################################################

dat_malaria_5to19 <- dat

# Save output(s)
write.csv(dat_malaria_5to19, paste("./gen/prediction/input/dth_malaria_5to19.csv", sep=""), row.names = FALSE)

###################################################################
######################### END-OUTPUTS #############################
###################################################################
