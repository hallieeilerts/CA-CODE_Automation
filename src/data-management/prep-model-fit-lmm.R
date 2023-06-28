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
if(ageGroup == "05to09"){load("./data/estimation/mod_fit/20210328-5to9-lam400SD007-VRMCM003-Test8b.RData")}
if(ageGroup == "10to14"){load("./data/estimation/mod_fit/20210327-10to14-lam300SD007-VRMCM003-Test6b.RData")}
if(ageGroup == "15to19f"){load("./data/estimation/mod_fit/20210328-15to19Fem-lam300SD007-VRMCM003-Test6g.RData")}
if(ageGroup == "15to19m"){load("./data/estimation/mod_fit/20210331-15to19Men-lam400SD007-VRMCM003-Test6e.RData")}
################################################################################

# Save output(s) ----------------------------------------------------------

saveRDS(out, file = paste("./gen/estimation/output/mod_fit_", ageGroup, "LMM.rds",sep=""))

# Remove unnecessary objects
rm(out, fileName, model, rateTrans, refCat, sex, test, Time, vers)
