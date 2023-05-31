###################################################################
########################## BEGIN-INPUTS ###########################
###################################################################

# Load packages and session variables if not already loaded
if(!exists("sessionVars")){source("./src/prepare-session.R")
  load("./gen/data-prep/input/session-variables.Rdata")}

# Load input(s)
# env_crisisfree_u20_igme <- read_excel("./data/envelopes/UN IGME 2022 Rates & Deaths_Country Summary (crisis free) 1980-2021 all ages.xlsx")
# Should we be using crisis-free or crisis-included u5m rate in 2010 to set country class?
# Need to load information on whether its a fragile state. Where does this come from?

###################################################################
########################## END-INPUTS #############################
###################################################################

# Add code that assigns countries to GOODVR, LMM or HMM
# As a place holder, I will just re-save 20201001-CountryModelClass.csv
# and use the updated name for this object (key_ctryclass_u20)
key_ctryclass_u20 <- read.csv("./gen/data-prep/temp/20201001-CountryModelClass.csv")
dat <- key_ctryclass_u20

# Remove unnecessary objects
rm(key_ctryclass_u20)

###################################################################
######################### BEGIN-OUTPUTS ###########################
###################################################################

# Save output(s)
write.csv(dat, "./gen/data-prep/output/key_ctryclass_u20.csv", row.names = FALSE)

###################################################################
######################### END-OUTPUTS #############################
###################################################################

