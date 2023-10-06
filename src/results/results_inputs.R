################################################################################
#' @description Loads all libraries and inputs required for Results
#' @return Inputs loaded below
################################################################################
#' Libraries
library(data.table) # melt, dcast
#' Inputs
source("./src/prepare-session/set-inputs.R")
source("./src/prepare-session/create-session-variables.R")

# Classification keys
key_cod           <- read.csv(paste("./gen/data-management/output/key_cod_", ageGroup, ".csv", sep=""))
key_region_u20    <- read.csv("./gen/data-management/output/key_region_u20.csv")
key_ctryclass_u20 <- read.csv("./gen/data-management/output/key_ctryclass_u20.csv")

# CSMFs that have been processed in squeezing pipeline (contains all countries, even those not subject to squeezing)
csmfSqz     <- read.csv(paste("./gen/squeezing/output/csmfSqz_", ageGroup, ".csv", sep = ""))
csmfSqz_REG <- read.csv(paste("./gen/squeezing/output/csmfSqz_", ageGroup, "REG.csv", sep = ""))

# Point estimates, lower, and upper bounds for fractions/deaths/rates that have been processed in uncertainty pipeline
# (some point estimates for fractions are different from csmfSqz because they were adjusted in fn_adjust_pointint() [Pancho's AdjustUncert() function])
pointInt     <- read.csv(paste("./gen/uncertainty/output/pointInt_", ageGroup, ".csv", sep = ""))
pointInt_REG <- read.csv(paste("./gen/uncertainty/output/pointInt_", ageGroup, "REG.csv", sep = ""))
################################################################################