
###############
#             #
# FULL UPDATE #
#             #
###############

##############
# Set inputs
##############

## Clear environment
rm(list = ls())

## Choose age/sex group
# ageGroup <- "00to28"
# ageGroup <- "01to04"
#ageGroup <- "05to09"
#ageGroup <- "10to14"
#ageGroup <- "15to19f"
 ageGroup <- "15to19m"
# ageGroup <- "05to19"
# ageGroup <- "10to19"
# ageGroup <- "15to19"

## Years for update
Years <- 2000:2021

## Prepare session
source("./src/prepare-session.R")
source("./src/create-session-variables.R")

################################################
# Data preparation
################################################

# Classification keys
source("./src/data-prep/set-cod-list.R")
source("./src/data-prep/set-regions.R")
source("./src/data-prep/set-country-class.R")

# Exposure and outcome data
source("./src/data-prep/prep-goodvr.R")
source("./src/data-prep/prep-prediction-database.R")
if(ageGroup %in% c("00to28","01to04")){source("./src/data-prep/prep-chinanmch.R")}
if(ageGroup %in% c("05to09","10to14","15to19f", "15to19m")){source("./src/data-prep/prep-chinadsp.R")}

# Envelopes
source("./src/data-prep/prep-envelopes.R")

# Single-cause data
source("./src/data-prep/prep-crisis.R")
source("./src/data-prep/prep-hiv.R")
source("./src/data-prep/prep-malaria.R")
source("./src/data-prep/prep-tb.R")
if(ageGroup == "05to09"){source("./src/data-prep/prep-measles.R")}

################################################
# Estimation
################################################

source("src/estimation/prep-model-input-hmm.R")
source("src/estimation/prep-model-input-lmm.R")
source("src/estimation/create-parameter-grid-hmm.R")
source("src/estimation/create-parameter-grid-lmm.R")
source("src/estimation/select-covariates-hmm.R")
source("src/estimation/select-covariates-lmm.R")
# This is the file that contains the results of the formal parameter selection is named Code/LASSO_VA002.txt in Pancho's code. Can't find this file though.
#source("src/estimation/formal-parameter-selection-hmm.R")
#source("src/estimation/formal-parameter-selection-lmm.R")
source("src/estimation/run-bayesian-lasso-hmm.R")
source("src/estimation/run-bayesian-lasso-lmm.R")

################################################
# Prediction
################################################

source("./src/prediction/calculate-csmf-goodvr.R")
source("./src/prediction/calculate-csmf-china.R")

source("./src/prediction/extract-cov.R")
source("./src/prediction/prediction_functions.R")
source("./src/prediction/predict-csmf.R")

if(ageGroup %in% c("05to09", "10to14")){
  source("./src/prediction/set-min-frac-malaria.R")
  source("./src/prediction/cap-malaria-hmm.R")
  source("./src/prediction/set-malaria-lmm.R")
}

################################################
# Squeezing
################################################

# Set minimum fractions
source("./src/squeezing/set-min-frac-cd.R")
if(ageGroup %in% c("05to09", "10to14")){source("./src/squeezing/set-min-frac-lri.R")}

# Prepare for squeezing
source("./src/squeezing/prep-squeezing.R")
source("./src/squeezing/prep-squeezing-china.R")

# if(ageGroup == "00to01"){
#   source(src/squeezing/split-sepsis.R)
# }
# if(ageGroup == "01to59"){
#   source(src/squeezing/split-perinatal.R)         # split perinatal into preterm and intrapartum
#   source(src/squeezing/adjust-vaccine.R)          # posthoc vaccine adjustment
#   source(src/squeezing/adjust-tb-underrec.R)      # reassigns excess pulmonary tb as carve out of lri
#   source(src/squeezing/squeeze-othercmpn.R)       # squeezing single causes into othercmpn
#   source(src/squeezing/squeeze-crisis-epidemic.R) # squeezing in epi crisis
#   source(src/squeezing/add-measles-epidemic.R)    # squeezing in epi meas
# }

if(ageGroup == "05to09"){
  source("./src/squeezing/sqz-othercmpn.R")
  source("./src/squeezing/sqz-othercmpn-china.R")
  source("./src/squeezing/sqz-lri.R")
  source("./src/squeezing/sqz-crisis-end.R")
  source("./src/squeezing/sqz-crisis-epi.R")
  source("./src/squeezing/add-measles-epi.R")
}
if(ageGroup == "10to14"){
  source("./src/squeezing/sqz-othercmpn.R")
  source("./src/squeezing/sqz-othercmpn-china.R")
  source("./src/squeezing/sqz-lri.R")
  source("./src/squeezing/sqz-crisis-end.R")
  source("./src/squeezing/sqz-crisis-epi.R")
}
if(ageGroup %in% c("15to19f", "15to19m")){
  source("./src/squeezing/sqz-othercmpn.R")
  source("./src/squeezing/sqz-othercmpn-china.R")
  source("./src/squeezing/sqz-crisis-end.R")
  source("./src/squeezing/sqz-crisis-epi.R")
}

source("./src/squeezing/format-squeezed-output.R")

################################################
# Results
################################################

source("./src/results/format-results.R")
source("./src/results/visualize-sample.R")

################################################
# Uncertainty
################################################


# Testing loop
rm(list = ls())
source("./src/prediction/functions_prediction.R")
v_ageGroup <- c("05to09", "10to14", "15to19f", "15to19m")
for(i in 1:length(v_ageGroup)){
  ## Choose age/sex group
  ageGroup <- v_ageGroup[i]
  print(ageGroup)
  ## Years for update
  Years <- 2000:2021
  ## Prepare session
  source("./src/prepare-session.R")
  source("./src/create-session-variables.R")
  ## Testing functions
  #if(ageGroup %in% c("05to09")){source("./src/data-prep/prep-measles.R")}
  #source("./src/data-prep/prep-crisis.R")
  #source("./src/data-prep/prep-hiv.R")
  #source("./src/data-prep/prep-malaria.R")
  #source("./src/data-prep/prep-tb.R")
  #source("src/estimation/select-covariates-hmm.R")
  #source("src/estimation/select-covariates-lmm.R")
  #source("src/estimation/run-bayesian-lasso-hmm.R")
  #source("src/estimation/run-bayesian-lasso-lmm.R")
  source("./src/squeezing/set-min-frac-cd.R")
  source("./src/squeezing/set-min-frac-lri.R")
  #source("./src/prediction/functions_prediction.R")
  #source("./src/prediction/predict-csmf.R")
  #source("./src/prediction/calculate-csmf-china.R")
  #if(ageGroup %in% c("05to09", "10to14")){
  #  source("./src/prediction/set-min-frac-malaria.R")
  #  source("./src/prediction/cap-malaria-hmm.R")
  #  source("./src/prediction/set-malaria-lmm.R")
  #}
  #source("./src/squeezing/prep-squeezing.R")
  #source("./src/squeezing/prep-squeezing-china.R")
  #source("./src/squeezing/sqz-othercmpn.R")
  #source("./src/squeezing/sqz-othercmpn-china.R")
  #if(ageGroup %in% c("05to09", "10to14")){source("./src/squeezing/sqz-lri.R")}
  #source("./src/squeezing/sqz-crisis-end.R")
  #source("./src/squeezing/sqz-crisis-epi.R")
  #source("./src/squeezing/add-measles-epi.R")
  #source("./src/squeezing/format-squeezed-output.R")
  #source("./src/results/format-results.R")
  #source("./src/results/visualize-sample.R")
}

