################################################
# Aggregated Point Estimates
################################################

# Clear environment
rm(list = ls())

# Load inputs and functions
source("./src/aggregation/aggregation_inputs.R")
source("./src/aggregation/aggregation_functions.R")

# Calculate aggregate point estimates -----------------------------------------------

# Need to have already calculated CSMFs for all standard age groups.

csmfSqz_05to14 <- fn_calcAggAges(AGELB = 5, AGEUB = 14, CODALL = codAll, CSMF_5TO9 = csmfSqz_05to09,  CSMF_10TO14 = csmfSqz_10to14)
csmfSqz_05to19 <- fn_calcAggAges(AGELB = 5, AGEUB = 19, CODALL = codAll, CSMF_5TO9 = csmfSqz_05to09,  CSMF_10TO14 = csmfSqz_10to14, CSMF_15TO19F = csmfSqz_15to19f, CSMF_15TO19M = csmfSqz_15to19m, ENV = env_u20)
csmfSqz_10to19 <- fn_calcAggAges(AGELB = 10, AGEUB = 19, CODALL = codAll, CSMF_10TO14 = csmfSqz_10to14, CSMF_15TO19F = csmfSqz_15to19f, CSMF_15TO19M = csmfSqz_15to19m, ENV = env_u20)
csmfSqz_15to19 <- fn_calcAggAges(AGELB = 15, AGEUB = 19, CODALL = codAll, CSMF_15TO19F = csmfSqz_15to19f, CSMF_15TO19M = csmfSqz_15to19m, ENV = env_u20)

# Save point estimates
write.csv(csmfSqz_05to14, paste("./gen/aggregation/temp/csmfSqz_05to14.csv", sep = ""))
write.csv(csmfSqz_05to19, paste("./gen/aggregation/temp/csmfSqz_05to19.csv", sep = ""))
write.csv(csmfSqz_10to19, paste("./gen/aggregation/temp/csmfSqz_10to19.csv", sep = ""))
write.csv(csmfSqz_15to19, paste("./gen/aggregation/temp/csmfSqz_15to19.csv", sep = ""))

# Calculate regional CSMFs  for aggregate age groups
csmfSqz_05to14REG <- fn_calcRegion(csmfSqz_05to14, NULL, codAll, key_region_u20)
csmfSqz_05to19REG <- fn_calcRegion(csmfSqz_05to19, NULL, codAll, key_region_u20)
csmfSqz_10to19REG <- fn_calcRegion(csmfSqz_10to19, env_10to14REG, codAll, key_region_u20)
csmfSqz_15to19REG <- fn_calcRegion(csmfSqz_15to19, env_15to19REG, codAll, key_region_u20)

# Save point estimates
write.csv(csmfSqz_05to14REG, paste("./gen/aggregation/temp/csmfSqz_05to14REG.csv", sep=""), row.names = FALSE)
write.csv(csmfSqz_05to19REG, paste("./gen/aggregation/temp/csmfSqz_05to19REG.csv", sep=""), row.names = FALSE)
write.csv(csmfSqz_10to19REG, paste("./gen/aggregation/temp/csmfSqz_10to19REG.csv", sep=""), row.names = FALSE)
write.csv(csmfSqz_15to19REG, paste("./gen/aggregation/temp/csmfSqz_15to19REG.csv", sep=""), row.names = FALSE)

# Publish point estimates from squeezing pipeline -------------------------------

# These are intermediate results while waiting on inputs for uncertainty pipeline.

# Load (if necessary)
# csmfSqz_05to14 <- read.csv("./gen/aggregation/temp/csmfSqz_05to14.csv")
# csmfSqz_05to19 <- read.csv("./gen/aggregation/temp/csmfSqz_05to19.csv")
# csmfSqz_10to19 <- read.csv("./gen/aggregation/temp/csmfSqz_10to19.csv")
# csmfSqz_15to19 <- read.csv("./gen/aggregation/temp/csmfSqz_15to19.csv")
# csmfSqz_05to14REG <- read.csv("./gen/aggregation/temp/csmfSqz_05to14REG.csv")
# csmfSqz_05to19REG <- read.csv("./gen/aggregation/temp/csmfSqz_05to19REG.csv")
# csmfSqz_10to19REG <- read.csv("./gen/aggregation/temp/csmfSqz_10to19REG.csv")
# csmfSqz_15to19REG <- read.csv("./gen/aggregation/temp/csmfSqz_15to19REG.csv")

# Perform rounding steps that occur in uncertainty pipeline
csmfSqz_ADJ_05to14 <- fn_adjustCSMFZeroDeaths(csmfSqz_05to14, codAll)
csmfSqz_ADJ_05to19 <- fn_adjustCSMFZeroDeaths(csmfSqz_05to19, codAll)
csmfSqz_ADJ_10to19 <- fn_adjustCSMFZeroDeaths(csmfSqz_10to19, codAll)
csmfSqz_ADJ_15to19 <- fn_adjustCSMFZeroDeaths(csmfSqz_15to19, codAll)
csmfSqz_ADJ_05to14REG <- fn_adjustCSMFZeroDeaths(csmfSqz_05to14REG, codAll)
csmfSqz_ADJ_05to19REG <- fn_adjustCSMFZeroDeaths(csmfSqz_05to19REG, codAll)
csmfSqz_ADJ_10to19REG <- fn_adjustCSMFZeroDeaths(csmfSqz_10to19REG, codAll)
csmfSqz_ADJ_15to19REG <- fn_adjustCSMFZeroDeaths(csmfSqz_15to19REG, codAll)
csmfSqz_FRMT_05to14 <- fn_roundCSMFsqz(csmfSqz_ADJ_05to14, codAll)
csmfSqz_FRMT_05to19 <- fn_roundCSMFsqz(csmfSqz_ADJ_05to19, codAll)
csmfSqz_FRMT_10to19 <- fn_roundCSMFsqz(csmfSqz_ADJ_10to19, codAll)
csmfSqz_FRMT_15to19 <- fn_roundCSMFsqz(csmfSqz_ADJ_15to19, codAll)
csmfSqz_FRMT_05to14REG <- fn_roundCSMFsqz(csmfSqz_ADJ_05to14REG, codAll)
csmfSqz_FRMT_05to19REG <- fn_roundCSMFsqz(csmfSqz_ADJ_05to19REG, codAll)
csmfSqz_FRMT_10to19REG <- fn_roundCSMFsqz(csmfSqz_ADJ_10to19REG, codAll)
csmfSqz_FRMT_15to19REG <- fn_roundCSMFsqz(csmfSqz_ADJ_15to19REG, codAll)

# Add age columns for aggregate age groups
csmfSqz_FRMT_05to14 <- fn_addAgeCol(csmfSqz_FRMT_05to14, 5, 14)
csmfSqz_FRMT_05to19 <- fn_addAgeCol(csmfSqz_FRMT_05to19, 5, 19)
csmfSqz_FRMT_10to19 <- fn_addAgeCol(csmfSqz_FRMT_10to19, 10, 19)
csmfSqz_FRMT_15to19 <- fn_addAgeCol(csmfSqz_FRMT_15to19, 15, 19)
csmfSqz_FRMT_05to14REG <- fn_addAgeCol(csmfSqz_FRMT_05to14REG, 5, 14)
csmfSqz_FRMT_05to19REG <- fn_addAgeCol(csmfSqz_FRMT_05to19REG, 5, 19)
csmfSqz_FRMT_10to19REG <- fn_addAgeCol(csmfSqz_FRMT_10to19REG, 10, 19)
csmfSqz_FRMT_15to19REG <- fn_addAgeCol(csmfSqz_FRMT_15to19REG, 15, 19)

# Format estimates
csmfSqz_PUB_05to14  <- fn_publishEstimates(csmfSqz_FRMT_05to14, key_region_u20, key_ctryclass_u20, codAll, UNCERTAINTY = FALSE)
csmfSqz_PUB_05to19  <- fn_publishEstimates(csmfSqz_FRMT_05to19, key_region_u20, key_ctryclass_u20, codAll, UNCERTAINTY = FALSE)
csmfSqz_PUB_10to19  <- fn_publishEstimates(csmfSqz_FRMT_10to19, key_region_u20, key_ctryclass_u20, codAll, UNCERTAINTY = FALSE)
csmfSqz_PUB_15to19  <- fn_publishEstimates(csmfSqz_FRMT_15to19, key_region_u20, key_ctryclass_u20, codAll, UNCERTAINTY = FALSE)
csmfSqz_PUB_05to14REG  <- fn_publishEstimates(csmfSqz_FRMT_05to14REG, key_region_u20, key_ctryclass_u20, codAll, UNCERTAINTY = FALSE, REGIONAL = TRUE)
csmfSqz_PUB_05to19REG  <- fn_publishEstimates(csmfSqz_FRMT_05to19REG, key_region_u20, key_ctryclass_u20, codAll, UNCERTAINTY = FALSE, REGIONAL = TRUE)
csmfSqz_PUB_10to19REG  <- fn_publishEstimates(csmfSqz_FRMT_10to19REG, key_region_u20, key_ctryclass_u20, codAll, UNCERTAINTY = FALSE, REGIONAL = TRUE)
csmfSqz_PUB_15to19REG  <- fn_publishEstimates(csmfSqz_FRMT_15to19REG, key_region_u20, key_ctryclass_u20, codAll, UNCERTAINTY = FALSE, REGIONAL = TRUE)

# Save
write.csv(csmfSqz_PUB_05to14, paste("./gen/aggregation/output/PointEstimates_National_05to14_", resDate, ".csv", sep=""), row.names = FALSE)
write.csv(csmfSqz_PUB_05to19, paste("./gen/aggregation/output/PointEstimates_National_05to19_", resDate, ".csv", sep=""), row.names = FALSE)
write.csv(csmfSqz_PUB_10to19, paste("./gen/aggregation/output/PointEstimates_National_10to19_", resDate, ".csv", sep=""), row.names = FALSE)
write.csv(csmfSqz_PUB_15to19, paste("./gen/aggregation/output/PointEstimates_National_15to19_", resDate, ".csv", sep=""), row.names = FALSE)
write.csv(csmfSqz_PUB_05to14REG, paste("./gen/aggregation/output/PointEstimates_Regional_05to14_", resDate, ".csv", sep=""), row.names = FALSE)
write.csv(csmfSqz_PUB_05to19REG, paste("./gen/aggregation/output/PointEstimates_Regional_05to19_", resDate, ".csv", sep=""), row.names = FALSE)
write.csv(csmfSqz_PUB_10to19REG, paste("./gen/aggregation/output/PointEstimates_Regional_10to19_", resDate, ".csv", sep=""), row.names = FALSE)
write.csv(csmfSqz_PUB_15to19REG, paste("./gen/aggregation/output/PointEstimates_Regional_15to19_", resDate, ".csv", sep=""), row.names = FALSE)
