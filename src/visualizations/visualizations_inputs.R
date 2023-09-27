################################################################################
#' @description Loads all libraries and inputs required for Visualizations
#' @return Inputs loaded below
################################################################################
#' Libraries
library(ggplot2)
library(dplyr)
library(gridExtra)
library(plyr) # dlply
library(data.table) # melt, dcast
#' Inputs
source("./src/prepare-session/set-inputs.R")
source("./src/prepare-session/create-session-variables.R")

# Point estimates that have been processed in uncertainty pipeline
# (some point estimates for fractions are slightly different from csmfSqz because they were adjusted in fn_adjust_pointint() [Pancho's AdjustUncert() function])
point <- read.csv(paste("./gen/results/output/PointEstimates_National_", ageGroup,"_", resDate, ".csv", sep=""))
point_REG <- read.csv(paste("./gen/results/output/PointEstimates_Regional_", ageGroup,"_", resDate, ".csv", sep=""))
# Point estimates, lower, and upper bounds for fractions/deaths/rates that have been processed in uncertainty pipeline
pointInt <- read.csv(paste("./gen/results/output/Uncertainty_National_", ageGroup, "_", resDate, ".csv", sep = ""))
pointInt_REG <- read.csv(paste("./gen/results/output/Uncertainty_Regional_", ageGroup, "_", resDate, ".csv", sep = ""))

# Pancho's point estimates from previous estimation round
if(ageGroup == "05to09"){point_PrevResults <- read.csv("./data/previous-results/PointEstimates_2000-2019_National_05to09.csv")
                         point_PrevResults_REG <- read.csv("./data/previous-results/PointEstimates_2000-2019_Regional_05to09.csv")}
if(ageGroup == "10to14"){point_PrevResults <- read.csv("./data/previous-results/PointEstimates_2000-2019_National_10to14.csv")
                         point_PrevResults_REG <- read.csv("./data/previous-results/PointEstimates_2000-2019_Regional_10to14.csv")}
if(ageGroup %in% c("15to19f", "15to19m")){point_PrevResults <- read.csv("./data/previous-results/PointEstimates_2000-2019_National_15to19.csv")
                                          point_PrevResults_REG <- read.csv("./data/previous-results/PointEstimates_2000-2019_Regional_15to19.csv")}

# Pancho's point estimates from current estimation round
if(ageGroup == "05to09"){point_PanchoResults <- read.csv("./data/previous-results/PointEstimates5to9-National.csv")
                         point_PanchoResults_REG <- read.csv("./data/previous-results/PointEstimates5to9-Regional.csv")
                         pointInt_PanchoResults <- read.csv("./data/previous-results/Uncertainty5to9-National.csv")}
if(ageGroup == "10to14"){point_PanchoResults <- read.csv("./data/previous-results/PointEstimates10to14-National.csv")
                         point_PanchoResults_REG <- read.csv("./data/previous-results/PointEstimates10to14-Regional.csv")
                         pointInt_PanchoResults <- read.csv("./data/previous-results/Uncertainty10to14-National.csv")}
if(ageGroup %in% c("15to19f", "15to19m")){
                         point_PanchoResults <- read.csv("./data/previous-results/PointEstimates15to19-National.csv")
                         point_PanchoResults_REG <- read.csv("./data/previous-results/PointEstimates15to19-Regional.csv")
                         pointInt_PanchoResults <- read.csv("./data/previous-results/Uncertainty15to19-National.csv")}

# Classification keys
key_cod <- read.csv(paste("./gen/data-management/output/key_cod_", ageGroup, ".csv", sep=""))
key_region <- read.csv("./gen/data-management/output/key_region_u20.csv")
key_ctryclass <- read.csv("./gen/data-management/output/key_ctryclass_u20.csv")
################################################################################