###################################################################
########################## BEGIN-INPUTS ###########################
###################################################################

# Load packages and session variables if not already loaded
if(!exists("sessionVars")){source("./src/prepare-session.R")
  load("./gen/data-prep/input/session-variables.Rdata")}

# Load packages for estimation
library(R2jags)
library(doParallel)

# Load input(s)
# if(ageGroup == "05to09"){load("./gen/estimation/output/mod_input_05to09LMM.RData")
#                          load("./gen/estimation/input/mod_grid_05to09LMM.RData")
#                         # mod_par <- read.table("./gen/estimation/output/mod_par_05to09LMM.txt", header = FALSE, sep = "", dec = ".")
#                         # This file provides the results of the LOOCV 
#                         # Don't see this file currently in Pancho's Mort5to19 folder. Should be located here: Code/LASSO_VA002.txt                      
# }
# if(ageGroup == "10to14"){load("./gen/estimation/output/mod_input_10to14LMM.RData")
#                          load("./gen/estimation/input/mod_grid_10to14LMM.RData")
#                         # mod_par <- read.table("./gen/estimation/output/mod_par_10to14LMM.txt", header = FALSE, sep = "", dec = ".")
# }
# if(ageGroup == "15to19f"){load("./gen/estimation/output/mod_input_15to19fLMM.RData")
#                           load("./gen/estimation/input/mod_grid_15to19fLMM.RData")
#                           # mod_par <- read.table("./gen/estimation/output/mod_par_15to19fLMM.txt", header = FALSE, sep = "", dec = ".")
# }
# if(ageGroup == "15to19m"){load("./gen/estimation/output/mod_input_15to19mLMM.RData")
#                           load("./gen/estimation/input/mod_grid_15to19mLMM.RData")
#                           # mod_par <- read.table("./gen/estimation/output/mod_par_15to19mLMM.txt", header = FALSE, sep = "", dec = ".")
# }

###################################################################
########################## END-INPUTS #############################
###################################################################

# niter <- 5000      # Number of iterations
# burn <- 0           # Burn-in
# thin <- 20          # Thinning factor
# nchas <- 4          # Number of chains
# 
# # Covarites
# vxf
# # Random effects ('grouping' category)
# vxr
# # Causes of death
# vdt
# 
# # Save output
# SAVE <- T
# 
# # Run the model
# Start1 <- Sys.time()
# for (i in lamb) {
#   for (j in resd) {
#     
#     # Estimation
#     Start <- Sys.time()
#     out <- f.e1(PARA = T, STUD = studies, DEAT = deaths, VDT = vdt, VXF = vxf, 
#                 VXR = vxr, NDUM = 0, MODL = modl, RSDL = j,
#                 LAMB = i, CHAS = nchas, ITER = niter, BURN = burn, THIN = thin, SAMC = T)
#     End <- Sys.time()
#     Time <- End - Start
#     
#     # Save output
#     fileName <- paste0(format(Sys.Date(), "%Y%m%d"), '-',
#                        ageGroup, sex, '-lam', 
#                        gsub("\\.", "", paste(i)), 'SD',
#                        gsub("\\.", "", paste(j)), '-', model, vers, '-', test)
#     if (SAVE) save(out, ctrygrp, sexSplit, sex, vers, Time, 
#                    ageLow, refCat, rateTrans, test, fileName,
#                    file = paste0('./gen/estimation/output/', fileName, '.RData'))
#     rm(out)
#   }
# }
# print(Sys.time() - Start1)

# The above code is for running the estimation
# Don't want to re-run now.
# As a place holder, I will just re-save model outputs using the updated names
if(ageGroup == "05to09"){load("./gen/estimation/temp/20210328-5to9-lam400SD007-VRMCM003-Test8b.RData")}
if(ageGroup == "10to14"){load("./gen/estimation/temp/20210327-10to14-lam300SD007-VRMCM003-Test6b.RData")}
if(ageGroup == "15to19f"){load("./gen/estimation/temp/20210328-15to19Fem-lam300SD007-VRMCM003-Test6g.RData")}
if(ageGroup == "15to19m"){load("./gen/estimation/temp/20210331-15to19Men-lam400SD007-VRMCM003-Test6e.RData")}

###################################################################
######################### BEGIN-OUTPUTS ###########################
###################################################################

# Save output(s)
save.image(paste("./gen/estimation/output/mod_fit_",ageGroup, "LMM.RData",sep=""))

###################################################################
######################### END-OUTPUTS #############################
###################################################################

