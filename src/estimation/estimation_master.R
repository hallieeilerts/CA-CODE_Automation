################################################
# Estimation
################################################

# Load inputs and functions
source("./src/estimation/estimation_inputs.R")
source("./src/estimation/estimation_functions.R")

if(!simpleUpdate){
  # Note: Need to update header of all these scripts.
  source("src/estimation/prep-model-input-hmm.R")
  source("src/estimation/prep-model-input-lmm.R")
  source("src/estimation/create-parameter-grid-hmm.R")
  source("src/estimation/create-parameter-grid-lmm.R")
  source("src/estimation/select-covariates-hmm.R")
  source("src/estimation/select-covariates-lmm.R")
  # The file that contains the results of the formal parameter selection is named Code/LASSO_VA002.txt in Pancho's code. Can't find this file though.
  #source("src/estimation/formal-parameter-selection-hmm.R")
  #source("src/estimation/formal-parameter-selection-lmm.R")
  source("src/estimation/run-bayesian-lasso-hmm.R")
  source("src/estimation/run-bayesian-lasso-lmm.R")
}

# Clear environment
rm(list = ls())