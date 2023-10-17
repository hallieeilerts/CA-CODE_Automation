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

# Envelopes
env_u20 <- read.csv("./gen/data-management/output/env_u20.csv")
# Envelope draws for 15-19 sexes combined
envDraws_15to19 <- readRDS("./gen/data-management/output/envDraws_15to19.rds")
# Sampling vectors for IGME envelope draws based on number of HMM/LMM draws
v_sample <- readRDS("./gen/uncertainty/temp/sampleDraws.rds")
# Regional envelopes provided by IGME
env_10to14REG <- read.csv(paste("./gen/data-management/output/env_10to14REG.csv", sep=""))
env_15to19REG <- read.csv(paste("./gen/data-management/output/env_15to19REG.csv", sep=""))

# CSMFs that have been processed in squeezing pipeline for all age groups
csmfSqz_05to09 <- read.csv(paste("./gen/squeezing/output/csmfSqz_05to09.csv", sep = ""))
csmfSqz_10to14 <- read.csv(paste("./gen/squeezing/output/csmfSqz_10to14.csv", sep = ""))
csmfSqz_15to19f <- read.csv(paste("./gen/squeezing/output/csmfSqz_15to19f.csv", sep = ""))
csmfSqz_15to19m <- read.csv(paste("./gen/squeezing/output/csmfSqz_15to19m.csv", sep = ""))

# CSMF draws that have been processed in squeezing pipeline for all age groups 
csmfSqzDraws_05to09  <- readRDS(paste("./gen/uncertainty/temp/csmfSqzDraws_05to09.rds", sep=""))
csmfSqzDraws_10to14  <- readRDS(paste("./gen/uncertainty/temp/csmfSqzDraws_10to14.rds", sep=""))
csmfSqzDraws_15to19f  <- readRDS(paste("./gen/uncertainty/temp/csmfSqzDraws_15to19f.rds", sep=""))
csmfSqzDraws_15to19m  <- readRDS(paste("./gen/uncertainty/temp/csmfSqzDraws_15to19m.rds", sep=""))

# Pancho's point estimates from current estimation round
csmfSqz_PanchoResults_10to19 <- read.csv("./data/previous-results/PointEstimates10to19-National.csv")
csmfSqz_PanchoResults_10to19REG <- read.csv("./data/previous-results/PointEstimates10to19-Regional.csv")

################################################################################