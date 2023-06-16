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
if(ageGroup == "05to09"){load("./gen/estimation/input/20210327-Data5to9-VRMCM003-Test8b.RData")}
if(ageGroup == "10to14"){load("./gen/estimation/input/20210327-Data10to14-VRMCM003-Test6b.RData")}
if(ageGroup == "15to19f"){load("./gen/estimation/input/20210327-Data15to19Fem-VRMCM003-Test6g.RData")}
if(ageGroup == "15to19m"){load("./gen/estimation/input/20210327-Data15to19Men-VRMCM003-Test6e.RData")}

###################################################################
######################### BEGIN-OUTPUTS ###########################
###################################################################

# Save output(s)
save.image(paste("./gen/estimation/input/mod_input_",ageGroup, "LMM.RData",sep=""))

# Remove unnecessary objects
rm(data.predict, deaths, studies, model, rateTrans, refCat, test, vdt, vers, vxf,
   vxr, yearCov, ageMort, sex)

###################################################################
######################### END-OUTPUTS #############################
###################################################################