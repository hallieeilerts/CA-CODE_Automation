
###################################################################
########################## BEGIN-INPUTS ###########################
###################################################################

# Load packages and session variables if not already loaded
if(!exists("sessionVars")){source("./src/prepare-session.R")
  load("./gen/data-prep/input/session-variables.Rdata")}

# Load input(s)
# dth_mal_5to19_map <- read.csv() # need to get raw malaria data

###################################################################
########################## END-INPUTS #############################
###################################################################

# Add code to create malaria_adol_2Nov2022.dta

# As a place holder, I will just convert malaria_adol_2Nov2022.dta into a csv
# and use the updated name for this object (dth_malaria_5to19)
dat <- read.dta13("./data/single-causes/malaria/malaria_adol_2Nov2022.dta")
names(dat)[names(dat) == "iso"] <- "ISO3"
names(dat)[names(dat) == "year"] <- "Year"
names(dat)[names(dat) == "malcases"] <- "dth_malaria_5to19"

# Select variables of interest
dat <- dat[, c("ISO3", "Year", "dth_malaria_5to19")]
rownames(dat) <- NULL

###################################################################
######################### BEGIN-OUTPUTS ###########################
###################################################################

# Save output(s)
write.csv(dat, paste("./gen/prediction/input/dth_malaria_5to19.csv", sep=""), row.names = FALSE)

###################################################################
######################### END-OUTPUTS #############################
###################################################################
