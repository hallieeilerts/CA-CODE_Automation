###################################################################
########################## BEGIN-INPUTS ###########################
###################################################################

# Load session variables
load('./gen/data-prep/input/session-variables.Rdata')
ls()

# Variables passed by the make file
# If just running this script alone, they need to be manually defined.
if(!exists('runmake')){
  Years <- 2000:2021  ## EDIT THIS LINE ##
  ageLow <- 5         ## EDIT THIS LINE ##
  ageUp <- ageLow + 4
  ageGroup <- paste0(ageLow, 'to', ageUp)
  sexSplit <- ifelse(ageLow == 15, TRUE, FALSE)
  if (sexSplit) {
    Sex <- c('F', 'M')
  } else Sex <- 'B'
}

# Check for required variables
if(!(exists('idVars') & 
     exists('Years') & 
     exists('ageLow') & 
     exists('ageUp') & 
     exists('ageGroup') &
     exists('sexSplit'))){
  stop('Required variables are missing')
}

# Load input(s)
# db_vr_5to19_who <- read.csv() # need to get raw WHO data

###################################################################
########################## END-INPUTS #############################
###################################################################

# Code to create 20210331-GoodVR-5to19.dta
# Presumably this was down by cleaning the raw WHO data
# As a place holder, I will just convert 20210331-GoodVR-5to19.dta into a csv
# and use the updated name for this object (db_vr_5to19GOODV)
dat <- read.dta13('./data/vr/20210331-GoodVR-5to19.dta', nonint.factors = T)
db_vr_5to19GOODVR <- dat

###################################################################
######################### BEGIN-OUTPUTS ###########################
###################################################################

# Save output(s)
write.csv(db_vr_5to19GOODVR, './gen/data-prep/output/db_vr_5to19GOODVR.csv', row.names = FALSE)

###################################################################
######################### END-OUTPUTS #############################
###################################################################
