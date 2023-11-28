################################################
# Results
################################################

# Load inputs and functions
source("./src/results/results_inputs.R")
source("./src/results/results_functions.R")

# Perform rounding steps that occur in uncertainty pipeline
csmfSqz_ADJ <- fn_adjustCSMFZeroDeaths(csmfSqz, codAll)
csmfSqz_FRMT <- fn_roundCSMFsqz(csmfSqz_ADJ, codAll)
csmfSqz_ADJ_REG <- fn_adjustCSMFZeroDeaths(csmfSqz_REG, codAll)
csmfSqz_FRMT_REG <- fn_roundCSMFsqz(csmfSqz_ADJ_REG, codAll)
# Publish estimates from squeezing pipeline (intermediate results while waiting on inputs for uncertainty pipeline)
csmfSqz_PUB      <- fn_publishEstimates(csmfSqz_FRMT, key_region_u20, key_ctryclass_u20, codAll, UNCERTAINTY = FALSE)
csmfSqz_PUB_REG  <- fn_publishEstimates(csmfSqz_FRMT_REG, key_region_u20, key_ctryclass_u20, codAll, UNCERTAINTY = FALSE, REGIONAL = TRUE)
write.csv(csmfSqz_PUB, paste("./gen/results/temp/PointEstimates_National_", ageGroup,"_", resDate, ".csv", sep=""), row.names = FALSE)
write.csv(csmfSqz_PUB_REG, paste("./gen/results/temp/PointEstimates_Regional_", ageGroup,"_", resDate, ".csv", sep=""), row.names = FALSE)

# Publish estimates from uncertainty pipeline (final results)
point_PUB        <- fn_publishEstimates(pointInt, key_region_u20, key_ctryclass_u20, codAll, UNCERTAINTY = FALSE)
pointInt_PUB     <- fn_publishEstimates(pointInt, key_region_u20, key_ctryclass_u20, codAll, UNCERTAINTY = TRUE)
point_PUB_REG    <- fn_publishEstimates(pointInt_REG, key_region_u20, key_ctryclass_u20, codAll, UNCERTAINTY = FALSE, REGIONAL = TRUE)
pointInt_PUB_REG <- fn_publishEstimates(pointInt_REG, key_region_u20, key_ctryclass_u20, codAll, UNCERTAINTY = TRUE, REGIONAL = TRUE)

write.csv(point_PUB, paste("./gen/results/output/PointEstimates_National_", ageGroup,"_", resDate, ".csv", sep=""), row.names = FALSE)
write.csv(pointInt_PUB, paste("./gen/results/output/Uncertainty_National_", ageGroup,"_", resDate, ".csv", sep=""), row.names = FALSE)
write.csv(point_PUB_REG, paste("./gen/results/output/PointEstimates_Regional_", ageGroup,"_", resDate, ".csv", sep=""), row.names = FALSE)
write.csv(pointInt_PUB_REG, paste("./gen/results/output/Uncertainty_Regional_", ageGroup,"_", resDate, ".csv", sep=""), row.names = FALSE)

# Clear environment
rm(list = ls())
