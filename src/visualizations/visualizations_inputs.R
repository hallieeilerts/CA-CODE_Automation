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

#' Estimates that have been processed in squeezing piepline and formatted in results (intermediate results)
#' Note: Use these for creating visualizations when uncertainty pipeline isn't ready.
#' Some point estimates for fractions will be slightly different from the results produced in the uncertainty pipeline.
#' The function fn_adjustCSMFZeroDeaths performs some adjustments that happen in fn_adjustPointIntZeroDeaths(), but not all. 
#point <- read.csv(paste("./gen/results/temp/PointEstimates_National_", ageGroup,"_", resDate, ".csv", sep=""))
#point_REG <- read.csv(paste("./gen/results/temp/PointEstimates_Regional_", ageGroup,"_", resDate, ".csv", sep=""))

#' Estimates that have been processed in uncertainty pipeline and formatted in results (final results)
point <- read.csv(paste("./gen/results/output/PointEstimates_National_", ageGroup,"_20231002.csv", sep=""))
point_REG <- read.csv(paste("./gen/results/output/PointEstimates_Regional_", ageGroup,"_20231002.csv", sep=""))
pointInt <- read.csv(paste("./gen/results/output/Uncertainty_National_", ageGroup, "_20231002.csv", sep = ""))
pointInt_REG <- read.csv(paste("./gen/results/output/Uncertainty_Regional_", ageGroup, "_20231002.csv", sep = ""))

#' Pancho's estimates from previous estimation round
if(ageGroup == "05to09"){point_PrevResults <- read.csv("./data/previous-results/2000-2019/PointEstimates5to9-National.csv")
                         point_PrevResults_REG <- read.csv("./data/previous-results/2000-2019/PointEstimates5to9-Regional.csv")}
if(ageGroup == "10to14"){point_PrevResults <- read.csv("./data/previous-results/2000-2019/PointEstimates10to14-National.csv")
                         point_PrevResults_REG <- read.csv("./data/previous-results/2000-2019/PointEstimates10to14-Regional.csv")}
if(ageGroup %in% c("15to19f", "15to19m")){point_PrevResults <- read.csv("./data/previous-results/2000-2019/PointEstimates15to19-National.csv")
                                          point_PrevResults_REG <- read.csv("./data/previous-results/2000-2019/PointEstimates15to19-Regional.csv")}

#' Pancho's estimates from current estimation round
if(ageGroup == "05to09"){point_PanchoResults <- read.csv("./data/previous-results/2000-2021/PointEstimates5to9-National.csv")
                         point_PanchoResults_REG <- read.csv("./data/previous-results/2000-2021/PointEstimates5to9-Regional.csv")
                         pointInt_PanchoResults <- read.csv("./data/previous-results/2000-2021/Uncertainty5to9-National.csv")}
if(ageGroup == "10to14"){point_PanchoResults <- read.csv("./data/previous-results/2000-2021/PointEstimates10to14-National.csv")
                         point_PanchoResults_REG <- read.csv("./data/previous-results/2000-2021/PointEstimates10to14-Regional.csv")
                         pointInt_PanchoResults <- read.csv("./data/previous-results/2000-2021/Uncertainty10to14-National.csv")}
if(ageGroup %in% c("15to19f", "15to19m")){
                         point_PanchoResults <- read.csv("./data/previous-results/2000-2021/PointEstimates15to19-National.csv")
                         point_PanchoResults_REG <- read.csv("./data/previous-results/2000-2021/PointEstimates15to19-Regional.csv")
                         pointInt_PanchoResults <- read.csv("./data/previous-results/2000-2021/Uncertainty15to19-National.csv")}
################################################################################