################################################################################
#' @description Loads all libraries and inputs required for Results
#' @return Inputs loaded below
################################################################################
#' Libraries
library(plyr)
#' Inputs
source("./src/prepare-session/set-inputs.R")
source("./src/prepare-session/create-session-variables.R")

# CSMFs: point estimates
csmf <- read.csv(paste("./gen/squeezing/output/csmf_", ageGroup, ".csv", sep = ""))

# CSMFs: uncertainty intervals
unc_csmf <- read.csv(paste("./gen/uncertainty/output/unc_csmf_", ageGroup, ".csv", sep = ""))

# Classification keys
key_cod <- read.csv(paste("./gen/data-management/output/key_cod_", ageGroup, ".csv", sep=""))
key_region <- read.csv("./gen/data-management/output/key_region_u20.csv")
key_ctryclass <- read.csv("./gen/data-management/output/key_ctryclass_u20.csv")
################################################################################