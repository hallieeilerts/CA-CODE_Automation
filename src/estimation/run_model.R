#############################################################################
#                                                                           #  
# ADOLESCENT CAUSES OF DEATH - VAMCM AND VRMCM                              #
#                                                                           #
# Created by: Francisco Villavicencio - fvillav1@jhu.edu                    #
# Related files: Functions.R                                                #
#                                                                           #
#                                                              January 2021 #
#############################################################################


############################################################
### WORKING DIRECTORY AND PACKAGES                       ###  
############################################################

# Clear workspace
rm(list=ls())

# Working directory
if (.Platform$OS.type == "unix") {
  setwd("~/Dropbox/Adolescent/")
} else setwd("C:/Users/FVillavicencio/Dropbox/Adolescent/")

# Packages
library(R2jags)
library(doParallel)


############################################################
### DATA                                                 ###  
############################################################

# Load data
# load('Data/5to9/20201217-Data5to9-VAMCM009-Test3.RData')
load('Data/5to9/20210114-Data5to9-VRMCM003-Test3.RData')
# load('Data/10to14/20201222-Data10to14-VAMCM009-Test8j.RData')
# load('Data/15to19/20210114-Data15to19Fem-VAMCM009-Test6c.RData')

# Functions
source(paste0('Code/FunctionsCOD.R'))

# Model parameters
modl <- "Code/LASSO_VA002.txt"    # File with model
niter <- 5000      # Number of iterations
burn <- 0           # Burn-in
thin <- 20          # Thinning factor
nchas <- 4          # Number of chains

# Covarites
vxf
# Random effects ('grouping' category)
vxr
# Causes of death
vdt

# Model parameters to be tested
lamb <- c(10, 50, 100)
resd <- .07

# Age Group
ageGroup <- paste0(ageLow, 'to', ageLow + 4)


############################################################
### ESTIMATION                                           ###  
############################################################

# Save output
SAVE <- T

# Run the model
Start1 <- Sys.time()
for (i in lamb) {
  for (j in resd) {
    
    # Estimation
    Start <- Sys.time()
    out <- f.e1(PARA = T, STUD = studies, DEAT = deaths, VDT = vdt, VXF = vxf, 
                VXR = vxr, NDUM = 0, MODL = modl, RSDL = j,
                LAMB = i, CHAS = nchas, ITER = niter, BURN = burn, THIN = thin, SAMC = T)
    End <- Sys.time()
    Time <- End - Start
    
    # Save output
    fileName <- paste0(format(Sys.Date(), "%Y%m%d"), '-',
                       ageGroup, sex, '-lam', 
                       gsub("\\.", "", paste(i)), 'SD',
                       gsub("\\.", "", paste(j)), '-', model, vers, '-', test)
    if (SAVE) save(out, model, sexSplit, sex, vers, Time, 
                   ageLow, refCat, rateTrans, test, fileName,
                   file = paste0('Results/', ageGroup, '/', fileName, '.RData'))
    rm(out)
  }
}
print(Sys.time() - Start1)

