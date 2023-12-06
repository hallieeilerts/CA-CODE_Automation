################################################
# Aggregated Uncertainty
################################################

# Clear environment
rm(list = ls())

# Load section inputs and functions
source("./src/aggregation/aggregation_inputs.R")
source("./src/aggregation/aggregation_functions.R")

# Envelope draws for 15-19 sexes combined
envDraws_15to19 <- readRDS("./gen/data-management/output/envDraws_15to19.rds")
# Sampling vectors for IGME envelope draws based on number of HMM/LMM draws
v_sample <- readRDS("./gen/uncertainty/temp/sampleDraws.rds")

# CSMF draws that have been processed in squeezing pipeline for all age groups 
csmfSqzDraws_05to09  <- readRDS(paste("./gen/uncertainty/temp/csmfSqzDraws_05to09.rds", sep=""))
csmfSqzDraws_10to14  <- readRDS(paste("./gen/uncertainty/temp/csmfSqzDraws_10to14.rds", sep=""))
csmfSqzDraws_15to19f  <- readRDS(paste("./gen/uncertainty/temp/csmfSqzDraws_15to19f.rds", sep=""))
csmfSqzDraws_15to19m  <- readRDS(paste("./gen/uncertainty/temp/csmfSqzDraws_15to19m.rds", sep=""))

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
medInt_ADJ_05to14 <- fn_roundPointInt(ui_05to14, codAll)
medInt_ADJ_05to19 <- fn_roundPointInt(ui_05to19, codAll)
medInt_ADJ_10to19 <- fn_roundPointInt(ui_10to19, codAll)
medInt_ADJ_15to19 <- fn_roundPointInt(ui_15to19, codAll)
medInt_ADJ_05to14REG <- fn_roundPointInt(ui_05to14REG, codAll, REGIONAL = TRUE)
medInt_ADJ_05to19REG <- fn_roundPointInt(ui_05to19REG, codAll, REGIONAL = TRUE)
medInt_ADJ_10to19REG <- fn_roundPointInt(ui_10to19REG, codAll, REGIONAL = TRUE)
medInt_ADJ_15to19REG <- fn_roundPointInt(ui_15to19REG, codAll, REGIONAL = TRUE)

# Add age columns for aggregate group groups
medInt_ADJ_05to14 <- fn_addAgeCol(medInt_ADJ_05to14, 5, 14)
medInt_ADJ_05to19 <- fn_addAgeCol(medInt_ADJ_05to19, 5, 19)
medInt_ADJ_10to19 <- fn_addAgeCol(medInt_ADJ_10to19, 10, 19)
medInt_ADJ_15to19 <- fn_addAgeCol(medInt_ADJ_15to19, 15, 19)
medInt_ADJ_05to14REG <- fn_addAgeCol(medInt_ADJ_05to14REG, 5, 14)
medInt_ADJ_05to19REG <- fn_addAgeCol(medInt_ADJ_05to19REG, 5, 19)
medInt_ADJ_10to19REG <- fn_addAgeCol(medInt_ADJ_10to19REG, 10, 19)
medInt_ADJ_15to19REG <- fn_addAgeCol(medInt_ADJ_15to19REG, 15, 19)

# Save
# Note: Typically temporary files don't have a results date. These do because want to keep track of which set of results were aggregated.
# Need to delete old files from this /temp folder every now and then.
write.csv(medInt_ADJ_05to14, paste("./gen/aggregation/temp/medInt_05to14_", resDate, ".csv", sep=""), row.names = FALSE)
write.csv(medInt_ADJ_05to19, paste("./gen/aggregation/temp/medInt_05to19_", resDate, ".csv", sep=""), row.names = FALSE)
write.csv(medInt_ADJ_10to19, paste("./gen/aggregation/temp/medInt_10to19_", resDate, ".csv", sep=""), row.names = FALSE)
write.csv(medInt_ADJ_15to19, paste("./gen/aggregation/temp/medInt_15to19_", resDate, ".csv", sep=""), row.names = FALSE)
write.csv(medInt_ADJ_05to14REG, paste("./gen/aggregation/temp/medInt_05to14REG_", resDate, ".csv", sep=""), row.names = FALSE)
write.csv(medInt_ADJ_05to19REG, paste("./gen/aggregation/temp/medInt_05to19REG_", resDate, ".csv", sep=""), row.names = FALSE)
write.csv(medInt_ADJ_10to19REG, paste("./gen/aggregation/temp/medInt_10to19REG_", resDate, ".csv", sep=""), row.names = FALSE)
write.csv(medInt_ADJ_15to19REG, paste("./gen/aggregation/temp/medInt_15to19REG_", resDate, ".csv", sep=""), row.names = FALSE)

