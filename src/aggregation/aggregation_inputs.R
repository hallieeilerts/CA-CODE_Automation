################################################################################
#' @description Loads all libraries and inputs required for Uncertainty
#' @return Inputs loaded below
################################################################################
#' Libraries
require(plyr)  # ldply()
require(data.table) # melt(), dcast()
#' Inputs
source("./src/prepare-session/set-inputs.R")
source("./src/prepare-session/create-session-variables.R")

# Classification keys
key_cod    <- read.csv(paste("./gen/data-management/output/key_cod_", ageGroup, ".csv", sep=""))
key_region <- read.csv("./gen/data-management/output/key_region_u20.csv")

# Envelopes
env_u20 <- read.csv("./gen/data-management/output/env_u20.csv")
# Envelope draws for 15-19 sexes combined
envDraws_15to19 <- readRDS("./gen/data-management/output/envDraws_15to19.rds")
# Sampling vectors for IGME envelope draws based on number of HMM/LMM draws
v_sample <- readRDS("./gen/uncertainty/temp/sampleDraws.rds")

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
################################################################################