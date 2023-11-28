################################################
# Data management
################################################

# Classification keys
source("./src/data-management/set-cod-list.R")
source("./src/data-management/set-regions.R")
source("./src/data-management/set-country-class.R")

# Envelopes
source("./src/data-management/prep-envelopes.R")
source("./src/data-management/prep-envelopes-regional.R")
source("./src/data-management/prep-envelopes-draws.R") # For 15-19f or 15-19m, creates draws for males, females, and both sexes combined

# Exposure data
source("./src/data-management/prep-prediction-database.R")

# Outcome data
source("./src/data-management/prep-goodvr.R")
if(ageGroup %in% c("00to28","01to04")){source("./src/data-management/prep-chinanmch.R")}
if(ageGroup %in% c("05to09","10to14","15to19f", "15to19m")){source("./src/data-management/prep-chinadsp.R")}

# Single-cause data
source("./src/data-management/prep-crisis.R")
source("./src/data-management/prep-hiv.R")
source("./src/data-management/prep-malaria.R")
source("./src/data-management/prep-tb.R")
if(ageGroup == "05to09"){source("./src/data-management/prep-measles.R")}

# Model objects
if(simpleUpdate){
  source("./src/data-management/prep-model-objects-hmm.R")
  source("./src/data-management/prep-model-objects-lmm.R")
}

# Set fractions for capping malaria, and minimum fractions for squeezing
source("./src/data-management/set-frac-cap-malaria.R")
source("./src/data-management/set-frac-min-cd.R")
source("./src/data-management/set-frac-min-lri.R")

# Clear environment
rm(list = ls())