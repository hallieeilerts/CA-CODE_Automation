###################################################################
########################## BEGIN-INPUTS ###########################
###################################################################

# Load packages and session variables if not already loaded
if(!exists("sessionVars")){source("./src/prepare-session.R")
  load("./gen/data-prep/input/session-variables.Rdata")}

# Load input(s)
# db_china_5to19_dsp <- read.csv() # need to get raw China data

###################################################################
########################## END-INPUTS #############################
###################################################################

# Add code to create 20210330-ChinaDSP.dta

# As a place holder, I will just convert 20210330-ChinaDSP.dta into a csv
# and use the updated name for this object (db_china_5to19)
dat <- read.dta13("./gen/data-prep/temp/20210330-ChinaDSP.dta", nonint.factors = T)

###################################################################
######################### BEGIN-OUTPUTS ###########################
###################################################################

# Save output(s)
write.csv(dat, paste("./gen/data-prep/output/db_china_5to19.csv", sep=""), row.names = FALSE)

###################################################################
######################### END-OUTPUTS #############################
###################################################################


