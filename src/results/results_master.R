################################################
# Results
################################################

# Load inputs and functions
source("./src/results/results_inputs.R")
source("./src/results/results_functions.R")

# Publish estimates from squeezing pipeline -------------------------------

# These are intermediate results while waiting on inputs for uncertainty pipeline.

# Perform rounding steps that occur in uncertainty pipeline
csmfSqz_ADJ <- fn_adjustCSMFZeroDeaths(csmfSqz, codAll)
csmfSqz_FRMT <- fn_roundCSMFsqz(csmfSqz_ADJ, codAll)
csmfSqz_ADJ_REG <- fn_adjustCSMFZeroDeaths(csmfSqz_REG, codAll)
csmfSqz_FRMT_REG <- fn_roundCSMFsqz(csmfSqz_ADJ_REG, codAll)

# Format estimates
csmfSqz_PUB      <- fn_publishEstimates(csmfSqz_FRMT, key_region_u20, key_ctryclass_u20, codAll, UNCERTAINTY = FALSE)
csmfSqz_PUB_REG  <- fn_publishEstimates(csmfSqz_FRMT_REG, key_region_u20, key_ctryclass_u20, codAll, UNCERTAINTY = FALSE, REGIONAL = TRUE)

# Save
write.csv(csmfSqz_PUB, paste("./gen/results/temp/PointEstimates_National_", ageGroup,"_", resDate, ".csv", sep=""), row.names = FALSE)
write.csv(csmfSqz_PUB_REG, paste("./gen/results/temp/PointEstimates_Regional_", ageGroup,"_", resDate, ".csv", sep=""), row.names = FALSE)


# Publish estimates from uncertainty pipeline -----------------------------

# These are the final results.

# Format estimates
point_PUB        <- fn_publishEstimates(pointInt, key_region_u20, key_ctryclass_u20, codAll, UNCERTAINTY = FALSE)
pointInt_PUB     <- fn_publishEstimates(pointInt, key_region_u20, key_ctryclass_u20, codAll, UNCERTAINTY = TRUE)
point_PUB_REG    <- fn_publishEstimates(pointInt_REG, key_region_u20, key_ctryclass_u20, codAll, UNCERTAINTY = FALSE, REGIONAL = TRUE)
pointInt_PUB_REG <- fn_publishEstimates(pointInt_REG, key_region_u20, key_ctryclass_u20, codAll, UNCERTAINTY = TRUE, REGIONAL = TRUE)

# Save
write.csv(point_PUB, paste("./gen/results/output/PointEstimates_National_", ageGroup,"_", resDate, ".csv", sep=""), row.names = FALSE)
write.csv(pointInt_PUB, paste("./gen/results/output/Uncertainty_National_", ageGroup,"_", resDate, ".csv", sep=""), row.names = FALSE)
write.csv(point_PUB_REG, paste("./gen/results/output/PointEstimates_Regional_", ageGroup,"_", resDate, ".csv", sep=""), row.names = FALSE)
write.csv(pointInt_PUB_REG, paste("./gen/results/output/Uncertainty_Regional_", ageGroup,"_", resDate, ".csv", sep=""), row.names = FALSE)

# Clear environment
rm(list = ls())
