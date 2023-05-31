###################################################################
########################## BEGIN-INPUTS ###########################
###################################################################

# Load packages and session variables if not already loaded
if(!exists("sessionVars")){source("./src/prepare-session.R")
  load("./gen/data-prep/input/session-variables.Rdata")}

# Load input(s)
if(ageGroup == "05to09"){load("./gen/estimation/output/mod_input_05to09LMM.RData")
  load("./gen/estimation/input/mod_grid_05to09LMM.RData")
  # mod_cov <- #
  # Currently covariates are in the model-input object. Need to separate out and include the code that was used to select them.                   
}
if(ageGroup == "10to14"){load("./gen/estimation/output/mod_input_10to14LMM.RData")
  load("./gen/estimation/input/mod_grid_10to14LMM.RData")
  # mod_cov <- #
  # Currently covariates are in the model-input object. Need to separate out and include the code that was used to select them.      
}
if(ageGroup == "15to19f"){load("./gen/estimation/output/mod_input_15to19fLMM.RData")
  load("./gen/estimation/input/mod_grid_15to19fLMM.RData")
  # mod_cov <- #
  # Currently covariates are in the model-input object. Need to separate out and include the code that was used to select them.      
}
if(ageGroup == "15to19m"){load("./gen/estimation/output/mod_input_15to19mLMM.RData")
  load("./gen/estimation/input/mod_grid_15to19mLMM.RData")
  # mod_cov <- #
  # Currently covariates are in the model-input object. Need to separate out and include the code that was used to select them.      
}

###################################################################
########################## END-INPUTS #############################
###################################################################

# Whatever code is in Code/LASSO_VA002.txt

###################################################################
######################### BEGIN-OUTPUTS ###########################
###################################################################

# Save output(s)
# write.table(paste("./gen/estimation/output/mod_par_", ageGroup, "LMM.txt",sep=""))

###################################################################
######################### END-OUTPUTS #############################
###################################################################