################################################################################
#' @description Resave model fit with standardized name
#' @return List of length 5 with "param", "Vars", "Studies", "Deaths", "output" 
#' out$param has model info
#' out$Vars has mean and sd of model covariates
#' out$Studies has studies in model
#' out$Deaths is null
#' out$output has parameters for beta and random effects
################################################################################
#' Libraries
#' Inputs
source("./src/prepare-session/set-inputs.R")
source("./src/prepare-session/create-session-variables.R")
if(ageGroup == "05to09"){load("./data/estimation/mod_fit/20210318-5to9-lam75SD007-VAMCM009-Test3.RData")}
if(ageGroup == "10to14"){load("./data/estimation/mod_fit/20201222-10to14-lam50SD007-VAMCM009-Test8j.RData")}
if(ageGroup == "15to19f"){load("./data/estimation/mod_fit/20210207-15to19Fem-lam50SD007-VAMCM009-Test9.RData")}
if(ageGroup == "15to19m"){load("./data/estimation/mod_fit/20210318-15to19Men-lam400SD007-VAMCM009-Test9e.RData")}
################################################################################

# Save output(s) ----------------------------------------------------------

saveRDS(out, file = paste("./gen/estimation/output/mod_fit_", ageGroup, "HMM.rds",sep=""))

# Remove unnecessary objects
rm(out, fileName, model, rateTrans, refCat, sex, test, Time, vers)
