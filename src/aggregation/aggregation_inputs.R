################################################################################
#' @description Loads all libraries and inputs required for Uncertainty
#' @return Inputs loaded below
################################################################################
#' Libraries
require(plyr)  # ldply(), dlply
library(dplyr) # bind_rows
require(data.table) # melt(), dcast()
require(ggplot2)
library(gridExtra)
#' Inputs
source("./src/prepare-session/set-inputs.R")
source("./src/prepare-session/create-session-variables.R")

# Classification keys
key_cod    <- read.csv(paste("./gen/data-management/output/key_cod_", ageGroup, ".csv", sep=""))
key_region_u20     <- read.csv("./gen/data-management/output/key_region_u20.csv")
key_ctryclass_u20  <- read.csv("./gen/data-management/output/key_ctryclass_u20.csv")

## Pancho's point estimates from current estimation round
#csmfSqz_PanchoResults_10to19 <- read.csv("./data/previous-results/2000-2021/PointEstimates10to19-National.csv")
#csmfSqz_PanchoResults_10to19REG <- read.csv("./data/previous-results/2000-2021/PointEstimates10to19-Regional.csv")

################################################################################