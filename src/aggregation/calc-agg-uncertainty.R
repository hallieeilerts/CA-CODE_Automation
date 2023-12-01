################################################
# Aggregated Uncertainty
################################################

# Clear environment
rm(list = ls())

# Load inputs and functions
source("./src/aggregation/aggregation_inputs.R")
source("./src/aggregation/aggregation_functions.R")

# Calculate aggregate age group medians, uncertainty intervals -----------------------------------------------

## Draws

# Sample from envelope draws for 15-19 sexes combined
envDraws_SAMP_15to19 <- fn_randDrawEnv(envDraws_15to19, v_sample$env)
rm(envDraws_15to19, v_sample)

## Squeezing

# csmfSqzDraws: squeezed output from modeled countries (HMM and LMM) and China with GOODVR

# Combine squeezed output for aggregate age groups
csmfSqzDraws_05to14 <- fn_callCalcAggAges(AGELB=5, AGEUB=14, CODALL=codAll,
                                          CSMF_5TO9 = csmfSqzDraws_05to09,
                                          CSMF_10TO14 = csmfSqzDraws_10to14,
                                          CSMF_15TO19F = csmfSqzDraws_15to19f, CSMF_15TO19M = csmfSqzDraws_15to19m,
                                          ENV = envDraws_SAMP_15to19, UNCERTAINTY = TRUE)
csmfSqzDraws_05to19 <- fn_callCalcAggAges(AGELB=5, AGEUB=19, CODALL=codAll,
                                          CSMF_5TO9 = csmfSqzDraws_05to09,
                                          CSMF_10TO14 = csmfSqzDraws_10to14,
                                          CSMF_15TO19F = csmfSqzDraws_15to19f, CSMF_15TO19M = csmfSqzDraws_15to19m,
                                          ENV = envDraws_SAMP_15to19, UNCERTAINTY = TRUE)
csmfSqzDraws_10to19 <- fn_callCalcAggAges(AGELB=10, AGEUB=19, CODALL=codAll,
                                          CSMF_5TO9 = csmfSqzDraws_05to09,
                                          CSMF_10TO14 = csmfSqzDraws_10to14,
                                          CSMF_15TO19F = csmfSqzDraws_15to19f, CSMF_15TO19M = csmfSqzDraws_15to19m,
                                          ENV = envDraws_SAMP_15to19, UNCERTAINTY = TRUE)
csmfSqzDraws_15to19 <- fn_callCalcAggAges(AGELB=15, AGEUB=19, CODALL=codAll,
                                          CSMF_5TO9 = csmfSqzDraws_05to09, 
                                          CSMF_10TO14 = csmfSqzDraws_10to14, 
                                          CSMF_15TO19F = csmfSqzDraws_15to19f, CSMF_15TO19M = csmfSqzDraws_15to19m, 
                                          ENV = envDraws_SAMP_15to19, UNCERTAINTY = TRUE)

# Calculate regional CSMFs for aggregate age groups
# Note: No regional envelope draws provided. If available, these would have been used to replace rates for 10-14 and 15-19.
csmfSqzDraws_05to14REG <- lapply(csmfSqzDraws_05to14, function(x){ fn_calcRegion(x,  ENV_REGION = NULL, codAll, key_region_u20) })
csmfSqzDraws_05to19REG <- lapply(csmfSqzDraws_05to19, function(x){ fn_calcRegion(x,  ENV_REGION = NULL, codAll, key_region_u20) })
csmfSqzDraws_10to19REG <- lapply(csmfSqzDraws_10to19, function(x){ fn_calcRegion(x,  ENV_REGION = NULL, codAll, key_region_u20) })
csmfSqzDraws_15to19REG <- lapply(csmfSqzDraws_15to19, function(x){ fn_calcRegion(x,  ENV_REGION = NULL, codAll, key_region_u20) })

## Uncertainty

# Calculate uncertainty intervals
ui_05to14 <- fn_calcUI(csmfSqzDraws_05to14, UI = 0.95, CODALL = codAll)
ui_05to19 <- fn_calcUI(csmfSqzDraws_05to19, UI = 0.95, CODALL = codAll)
ui_10to19 <- fn_calcUI(csmfSqzDraws_10to19, UI = 0.95, CODALL = codAll)
ui_15to19 <- fn_calcUI(csmfSqzDraws_15to19, UI = 0.95, CODALL = codAll)
ui_05to14REG <- fn_calcUI(csmfSqzDraws_05to14REG, UI = 0.95, CODALL = codAll, REGIONAL = TRUE)
ui_05to19REG <- fn_calcUI(csmfSqzDraws_05to19REG, UI = 0.95, CODALL = codAll, REGIONAL = TRUE)
ui_10to19REG <- fn_calcUI(csmfSqzDraws_10to19REG, UI = 0.95, CODALL = codAll, REGIONAL = TRUE)
ui_15to19REG <- fn_calcUI(csmfSqzDraws_15to19REG, UI = 0.95, CODALL = codAll, REGIONAL = TRUE)

# Round median estimates with uncertainty intervals
pointInt_FRMT_05to14 <- fn_roundPointInt(ui_05to14, codAll)
pointInt_FRMT_05to19 <- fn_roundPointInt(ui_05to19, codAll)
pointInt_FRMT_10to19 <- fn_roundPointInt(ui_10to19, codAll)
pointInt_FRMT_15to19 <- fn_roundPointInt(ui_15to19, codAll)
pointInt_FRMT_05to14REG <- fn_roundPointInt(ui_05to14REG, codAll, REGIONAL = TRUE)
pointInt_FRMT_05to19REG <- fn_roundPointInt(ui_05to19REG, codAll, REGIONAL = TRUE)
pointInt_FRMT_10to19REG <- fn_roundPointInt(ui_10to19REG, codAll, REGIONAL = TRUE)
pointInt_FRMT_15to19REG <- fn_roundPointInt(ui_15to19REG, codAll, REGIONAL = TRUE)

# Audit: confirm that median estimates fall in uncertainty bounds
pointInt_AUD_05to14 <- fn_checkUI(pointInt_FRMT_05to14, codAll, QUANTILE = "median")
pointInt_AUD_05to19 <- fn_checkUI(pointInt_FRMT_05to19, codAll, QUANTILE = "median")
pointInt_AUD_10to19 <- fn_checkUI(pointInt_FRMT_10to19, codAll, QUANTILE = "median")
pointInt_AUD_15to19 <- fn_checkUI(pointInt_FRMT_15to19, codAll, QUANTILE = "median")
pointInt_AUD_05to14REG <- fn_checkUI(pointInt_FRMT_05to14REG, codAll, QUANTILE = "median", REGIONAL = TRUE)
pointInt_AUD_05to19REG <- fn_checkUI(pointInt_FRMT_05to19REG, codAll, QUANTILE = "median", REGIONAL = TRUE)
pointInt_AUD_10to19REG <- fn_checkUI(pointInt_FRMT_10to19REG, codAll, QUANTILE = "median", REGIONAL = TRUE)
pointInt_AUD_15to19REG <- fn_checkUI(pointInt_FRMT_15to19REG, codAll, QUANTILE = "median", REGIONAL = TRUE)
sum(unlist(lapply(list(pointInt_AUD_05to14, pointInt_AUD_05to19, pointInt_AUD_10to19, pointInt_AUD_15to19, 
                       pointInt_AUD_05to14REG, pointInt_AUD_05to19REG, pointInt_AUD_10to19REG, pointInt_AUD_15to19REG), nrow)))

# Add age columns for aggregate group groups
pointInt_FRMT_05to14 <- fn_addAgeCol(pointInt_FRMT_05to14, 5, 14)
pointInt_FRMT_05to19 <- fn_addAgeCol(pointInt_FRMT_05to19, 5, 19)
pointInt_FRMT_10to19 <- fn_addAgeCol(pointInt_FRMT_10to19, 10, 19)
pointInt_FRMT_15to19 <- fn_addAgeCol(pointInt_FRMT_15to19, 15, 19)
pointInt_FRMT_05to14REG <- fn_addAgeCol(pointInt_FRMT_05to14REG, 5, 14)
pointInt_FRMT_05to19REG <- fn_addAgeCol(pointInt_FRMT_05to19REG, 5, 19)
pointInt_FRMT_10to19REG <- fn_addAgeCol(pointInt_FRMT_10to19REG, 10, 19)
pointInt_FRMT_15to19REG <- fn_addAgeCol(pointInt_FRMT_15to19REG, 15, 19)

# Save
write.csv(pointInt_FRMT_05to14, paste("./gen/aggregation/temp/pointInt_05to14.csv", sep=""), row.names = FALSE)
write.csv(pointInt_FRMT_05to19, paste("./gen/aggregation/temp/pointInt_05to19.csv", sep=""), row.names = FALSE)
write.csv(pointInt_FRMT_10to19, paste("./gen/aggregation/temp/pointInt_10to19.csv", sep=""), row.names = FALSE)
write.csv(pointInt_FRMT_15to19, paste("./gen/aggregation/temp/pointInt_15to19.csv", sep=""), row.names = FALSE)
write.csv(pointInt_FRMT_05to14REG, paste("./gen/aggregation/temp/pointInt_05to14REG.csv", sep=""), row.names = FALSE)
write.csv(pointInt_FRMT_05to19REG, paste("./gen/aggregation/temp/pointInt_05to19REG.csv", sep=""), row.names = FALSE)
write.csv(pointInt_FRMT_10to19REG, paste("./gen/aggregation/temp/pointInt_10to19REG.csv", sep=""), row.names = FALSE)
write.csv(pointInt_FRMT_15to19REG, paste("./gen/aggregation/temp/pointInt_15to19REG.csv", sep=""), row.names = FALSE)

# Publish estimates from uncertainty pipeline -----------------------------

## HAVENT FINALIZED THIS SECTION

# Load (if necessary)
# pointInt_FRMT_05to14 <- read.csv(paste("./gen/aggregation/temp/pointInt_05to14.csv", sep = ""))
# pointInt_FRMT_05to19 <- read.csv(paste("./gen/aggregation/temp/pointInt_05to19.csv", sep = ""))
# pointInt_FRMT_10to19 <- read.csv(paste("./gen/aggregation/temp/pointInt_10to19.csv", sep = ""))
# pointInt_FRMT_15to19 <- read.csv(paste("./gen/aggregation/temp/pointInt_15to19.csv", sep = ""))
# pointInt_FRMT_05to14REG <- read.csv(paste("./gen/aggregation/temp/pointInt_05to14REG.csv", sep = ""))
# pointInt_FRMT_05to19REG <- read.csv(paste("./gen/aggregation/temp/pointInt_05to19REG.csv", sep = ""))
# pointInt_FRMT_10to19REG <- read.csv(paste("./gen/aggregation/temp/pointInt_10to19REG.csv", sep = ""))
# pointInt_FRMT_15to19REG <- read.csv(paste("./gen/aggregation/temp/pointInt_15to19REG.csv", sep = ""))

# Format estimates
point_PUB_05to14 <- fn_publishEstimates(pointInt_FRMT_05to14, key_region_u20, key_ctryclass_u20, codAll, UNCERTAINTY = FALSE)
point_PUB_05to19 <- fn_publishEstimates(pointInt_FRMT_05to19, key_region_u20, key_ctryclass_u20, codAll, UNCERTAINTY = FALSE)
point_PUB_10to19 <- fn_publishEstimates(pointInt_FRMT_10to19, key_region_u20, key_ctryclass_u20, codAll, UNCERTAINTY = FALSE)
point_PUB_15to19 <- fn_publishEstimates(pointInt_FRMT_15to19, key_region_u20, key_ctryclass_u20, codAll, UNCERTAINTY = FALSE)
point_PUB_05to14REG <- fn_publishEstimates(pointInt_FRMT_05to14REG, key_region_u20, key_ctryclass_u20, codAll, UNCERTAINTY = FALSE, REGIONAL = TRUE)
point_PUB_05to19REG <- fn_publishEstimates(pointInt_FRMT_05to19REG, key_region_u20, key_ctryclass_u20, codAll, UNCERTAINTY = FALSE, REGIONAL = TRUE)
point_PUB_10to19REG <- fn_publishEstimates(pointInt_FRMT_10to19REG, key_region_u20, key_ctryclass_u20, codAll, UNCERTAINTY = FALSE, REGIONAL = TRUE)
point_PUB_15to19REG <- fn_publishEstimates(pointInt_FRMT_15to19REG, key_region_u20, key_ctryclass_u20, codAll, UNCERTAINTY = FALSE, REGIONAL = TRUE)
pointInt_PUB_05to14 <- fn_publishEstimates(pointInt_FRMT_05to14, key_region_u20, key_ctryclass_u20, codAll, UNCERTAINTY = TRUE)
pointInt_PUB_05to19 <- fn_publishEstimates(pointInt_FRMT_05to19, key_region_u20, key_ctryclass_u20, codAll, UNCERTAINTY = TRUE)
pointInt_PUB_10to19 <- fn_publishEstimates(pointInt_FRMT_10to19, key_region_u20, key_ctryclass_u20, codAll, UNCERTAINTY = TRUE)
pointInt_PUB_15to19 <- fn_publishEstimates(pointInt_FRMT_15to19, key_region_u20, key_ctryclass_u20, codAll, UNCERTAINTY = TRUE)
pointInt_PUB_05to14REG <- fn_publishEstimates(pointInt_FRMT_05to14REG, key_region_u20, key_ctryclass_u20, codAll, UNCERTAINTY = TRUE, REGIONAL = TRUE)
pointInt_PUB_05to19REG <- fn_publishEstimates(pointInt_FRMT_05to19REG, key_region_u20, key_ctryclass_u20, codAll, UNCERTAINTY = TRUE, REGIONAL = TRUE)
pointInt_PUB_10to19REG <- fn_publishEstimates(pointInt_FRMT_10to19REG, key_region_u20, key_ctryclass_u20, codAll, UNCERTAINTY = TRUE, REGIONAL = TRUE)
pointInt_PUB_15to19REG <- fn_publishEstimates(pointInt_FRMT_15to19REG, key_region_u20, key_ctryclass_u20, codAll, UNCERTAINTY = TRUE, REGIONAL = TRUE)

# Save
write.csv(point_PUB_05to14, paste("./gen/aggregation/output/PointEstimates_National_05to14_", resDate, ".csv", sep=""), row.names = FALSE)
write.csv(point_PUB_05to19, paste("./gen/aggregation/output/PointEstimates_National_05to19_", resDate, ".csv", sep=""), row.names = FALSE)
write.csv(point_PUB_10to19, paste("./gen/aggregation/output/PointEstimates_National_10to19_", resDate, ".csv", sep=""), row.names = FALSE)
write.csv(point_PUB_15to19, paste("./gen/aggregation/output/PointEstimates_National_15to19_", resDate, ".csv", sep=""), row.names = FALSE)
write.csv(point_PUB_05to14REG, paste("./gen/aggregation/output/PointEstimates_Regional_05to14_", resDate, ".csv", sep=""), row.names = FALSE)
write.csv(point_PUB_05to19REG, paste("./gen/aggregation/output/PointEstimates_Regional_05to19_", resDate, ".csv", sep=""), row.names = FALSE)
write.csv(point_PUB_10to19REG, paste("./gen/aggregation/output/PointEstimates_Regional_10to19_", resDate, ".csv", sep=""), row.names = FALSE)
write.csv(point_PUB_15to19REG, paste("./gen/aggregation/output/PointEstimates_Regional_15to19_", resDate, ".csv", sep=""), row.names = FALSE)
write.csv(pointInt_PUB_05to14, paste("./gen/aggregation/output/Uncertainty_National_05to14_", resDate, ".csv", sep=""), row.names = FALSE)
write.csv(pointInt_PUB_05to19, paste("./gen/aggregation/output/Uncertainty_National_05to19_", resDate, ".csv", sep=""), row.names = FALSE)
write.csv(pointInt_PUB_10to19, paste("./gen/aggregation/output/Uncertainty_National_10to19_", resDate, ".csv", sep=""), row.names = FALSE)
write.csv(pointInt_PUB_15to19, paste("./gen/aggregation/output/Uncertainty_National_15to19_", resDate, ".csv", sep=""), row.names = FALSE)
write.csv(pointInt_PUB_05to14REG, paste("./gen/aggregation/output/Uncertainty_Regional_05to14_", resDate, ".csv", sep=""), row.names = FALSE)
write.csv(pointInt_PUB_05to19REG, paste("./gen/aggregation/output/Uncertainty_Regional_05to19_", resDate, ".csv", sep=""), row.names = FALSE)
write.csv(pointInt_PUB_10to19REG, paste("./gen/aggregation/output/Uncertainty_Regional_10to19_", resDate, ".csv", sep=""), row.names = FALSE)
write.csv(pointInt_PUB_15to19REG, paste("./gen/aggregation/output/Uncertainty_Regional_15to19_", resDate, ".csv", sep=""), row.names = FALSE)


