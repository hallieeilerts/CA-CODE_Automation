################################################################################
#' @description Loads all functions required for Aggregation.
#' @return Functions loaded below
################################################################################
source("./src/aggregation/fn_calcAggAges.R")
source("./src/aggregation/fn_callCalcAggAges.R")
source("./src/aggregation/fn_addAgeCol.R")
source("./src/aggregation/fn_formatAggPointEst.R")
source("./src/aggregation/fn_combineAggPointEstWithUI.R")
source("./src/squeezing/fn_calcRegion.R")
source("./src/uncertainty/fn_randDrawEnv.R")
source("./src/uncertainty/fn_calcUI.R")
source("./src/uncertainty/fn_roundPointInt.R")
source("./src/uncertainty/fn_checkUI.R")
source("./src/uncertainty/fn_adjustPointIntZeroDeaths.R")
source("./src/results/fn_adjustCSMFZeroDeaths.R")
source("./src/results/fn_roundCSMFsqz.R")
source("./src/results/fn_publishEstimates.R")
source("./src/visualizations/fn_reshapePanchoRegAndAgg.R")
source("./src/visualizations/fn_compareCSMF.R")
source("./src/visualizations/fn_plotSingleCSMF.R")
source("./src/visualizations/fn_plotSingleUI.R")
################################################################################








