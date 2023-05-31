###################################################################
########################## BEGIN-INPUTS ###########################
###################################################################

# Load packages and session variables if not already loaded
if(!exists("sessionVars")){source("./src/prepare-session.R")
  load("./gen/data-prep/input/session-variables.Rdata")}

# Load input(s)
### 

###################################################################
########################## END-INPUTS #############################
###################################################################

# Add code that creates model inputs
# As a place holder, I will just re-save model inputs using the updated names
if(ageGroup == "05to09"){load("./gen/estimation/input/20201217-Data5to9-VAMCM009-Test3.RData")}
if(ageGroup == "10to14"){load("./gen/estimation/input/20201222-Data10to14-VAMCM009-Test8j.RData")}
if(ageGroup == "15to19f"){load("./gen/estimation/input/20210207-Data15to19Fem-VAMCM009-Test9.RData")}
if(ageGroup == "15to19m"){load("./gen/estimation/input/20210212-Data15to19Men-VAMCM009-Test9e.RData")}

###################################################################
######################### BEGIN-OUTPUTS ###########################
###################################################################

# Save output(s)
save.image(paste("./gen/estimation/input/mod_input_",ageGroup, "HMM.RData",sep=""))

###################################################################
######################### END-OUTPUTS #############################
###################################################################