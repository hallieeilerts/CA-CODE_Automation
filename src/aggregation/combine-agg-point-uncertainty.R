################################################
# Combine aggregated point estimates and uncertainty
################################################

# Clear environment
rm(list = ls())

# Load section inputs and functions
source("./src/aggregation/aggregation_inputs.R")
source("./src/aggregation/aggregation_functions.R")

# Set results date
aggResDate <- "20231201"

# Load point estimates
csmfSqz_05to14 <- read.csv(paste("./gen/aggregation/temp/csmfSqz_05to14_",aggResDate,".csv", sep = ""))
csmfSqz_05to19 <- read.csv(paste("./gen/aggregation/temp/csmfSqz_05to19_",aggResDate,".csv", sep = ""))
csmfSqz_10to19 <- read.csv(paste("./gen/aggregation/temp/csmfSqz_10to19_",aggResDate,".csv", sep = ""))
csmfSqz_15to19 <- read.csv(paste("./gen/aggregation/temp/csmfSqz_15to19_",aggResDate,".csv", sep = ""))
csmfSqz_05to14REG <- read.csv(paste("./gen/aggregation/temp/csmfSqz_05to14REG_",aggResDate,".csv", sep = ""))
csmfSqz_05to19REG <- read.csv(paste("./gen/aggregation/temp/csmfSqz_05to19REG_",aggResDate,".csv", sep = ""))
csmfSqz_10to19REG <- read.csv(paste("./gen/aggregation/temp/csmfSqz_10to19REG_",aggResDate,".csv", sep = ""))
csmfSqz_15to19REG <- read.csv(paste("./gen/aggregation/temp/csmfSqz_15to19REG_",aggResDate,".csv", sep = ""))

# Load uncertainty intervals and medians
medInt_05to14 <- read.csv(paste("./gen/aggregation/temp/medInt_05to14_",aggResDate,".csv", sep = ""))
medInt_05to19 <- read.csv(paste("./gen/aggregation/temp/medInt_05to19_",aggResDate,".csv", sep = ""))
medInt_10to19 <- read.csv(paste("./gen/aggregation/temp/medInt_10to19_",aggResDate,".csv", sep = ""))
medInt_15to19 <- read.csv(paste("./gen/aggregation/temp/medInt_15to19_",aggResDate,".csv", sep = ""))
medInt_05to14REG <- read.csv(paste("./gen/aggregation/temp/medInt_05to14REG_",aggResDate,".csv", sep = ""))
medInt_05to19REG <- read.csv(paste("./gen/aggregation/temp/medInt_05to19REG_",aggResDate,".csv", sep = ""))
medInt_10to19REG <- read.csv(paste("./gen/aggregation/temp/medInt_10to19REG_",aggResDate,".csv", sep = ""))
medInt_15to19REG <- read.csv(paste("./gen/aggregation/temp/medInt_15to19REG_",aggResDate,".csv", sep = ""))

# Combine point estimates with uncertainty intervals  --------------------------------------------------

# Format point estimates into same format as uncertainty
csmfSqz_FRMT_05to14 <- fn_formatAggPointEst(csmfSqz_05to14, codAll)
csmfSqz_FRMT_05to19 <- fn_formatAggPointEst(csmfSqz_05to19, codAll)
csmfSqz_FRMT_10to19 <- fn_formatAggPointEst(csmfSqz_10to19, codAll)
csmfSqz_FRMT_15to19 <- fn_formatAggPointEst(csmfSqz_15to19, codAll)
csmfSqz_FRMT_05to14REG <- fn_formatAggPointEst(csmfSqz_05to14REG, codAll, REGIONAL = TRUE)
csmfSqz_FRMT_05to19REG <- fn_formatAggPointEst(csmfSqz_05to19REG, codAll, REGIONAL = TRUE)
csmfSqz_FRMT_10to19REG <- fn_formatAggPointEst(csmfSqz_10to19REG, codAll, REGIONAL = TRUE)
csmfSqz_FRMT_15to19REG <- fn_formatAggPointEst(csmfSqz_15to19REG, codAll, REGIONAL = TRUE)

# Round median estimates with uncertainty intervals
csmfSqz_ADJ_05to14 <- fn_roundPointInt(csmfSqz_FRMT_05to14, codAll)
csmfSqz_ADJ_05to19 <- fn_roundPointInt(csmfSqz_FRMT_05to19, codAll)
csmfSqz_ADJ_10to19 <- fn_roundPointInt(csmfSqz_FRMT_10to19, codAll)
csmfSqz_ADJ_15to19 <- fn_roundPointInt(csmfSqz_FRMT_15to19, codAll)
csmfSqz_ADJ_05to14REG <- fn_roundPointInt(csmfSqz_FRMT_05to14REG, codAll, REGIONAL = TRUE)
csmfSqz_ADJ_05to19REG <- fn_roundPointInt(csmfSqz_FRMT_05to19REG, codAll, REGIONAL = TRUE)
csmfSqz_ADJ_10to19REG <- fn_roundPointInt(csmfSqz_FRMT_10to19REG, codAll, REGIONAL = TRUE)
csmfSqz_ADJ_15to19REG <- fn_roundPointInt(csmfSqz_FRMT_15to19REG, codAll, REGIONAL = TRUE)

# Add age columns for aggregate group groups
csmfSqz_FRMT2_05to14 <- fn_addAgeCol(csmfSqz_ADJ_05to14, 5, 14)
csmfSqz_FRMT2_05to19 <- fn_addAgeCol(csmfSqz_ADJ_05to19, 5, 19)
csmfSqz_FRMT2_10to19 <- fn_addAgeCol(csmfSqz_ADJ_10to19, 10, 19)
csmfSqz_FRMT2_15to19 <- fn_addAgeCol(csmfSqz_ADJ_15to19, 15, 19)
csmfSqz_FRMT2_05to14REG <- fn_addAgeCol(csmfSqz_ADJ_05to14REG, 5, 14)
csmfSqz_FRMT2_05to19REG <- fn_addAgeCol(csmfSqz_ADJ_05to19REG, 5, 19)
csmfSqz_FRMT2_10to19REG <- fn_addAgeCol(csmfSqz_ADJ_10to19REG, 10, 19)
csmfSqz_FRMT2_15to19REG <- fn_addAgeCol(csmfSqz_ADJ_15to19REG, 15, 19)

# Combine
pointInt_05to14 <- fn_combineAggPointEstWithUI(csmfSqz_FRMT2_05to14, medInt_05to14)
pointInt_05to19 <- fn_combineAggPointEstWithUI(csmfSqz_FRMT2_05to19, medInt_05to19)
pointInt_10to19 <- fn_combineAggPointEstWithUI(csmfSqz_FRMT2_10to19, medInt_10to19)
pointInt_15to19 <- fn_combineAggPointEstWithUI(csmfSqz_FRMT2_15to19, medInt_15to19)
pointInt_05to14REG <- fn_combineAggPointEstWithUI(csmfSqz_FRMT2_05to14REG, medInt_05to14REG, REGIONAL = TRUE)
pointInt_05to19REG <- fn_combineAggPointEstWithUI(csmfSqz_FRMT2_05to19REG, medInt_05to19REG, REGIONAL = TRUE)
pointInt_10to19REG <- fn_combineAggPointEstWithUI(csmfSqz_FRMT2_10to19REG, medInt_10to19REG, REGIONAL = TRUE)
pointInt_15to19REG <- fn_combineAggPointEstWithUI(csmfSqz_FRMT2_15to19REG, medInt_15to19REG, REGIONAL = TRUE)

# Audit -------------------------------------------------------------------

# Check if point estimates fall in uncertainty bounds
pointInt_AUD_05to14 <- fn_checkUI(pointInt_05to14, codAll)
pointInt_AUD_05to19 <- fn_checkUI(pointInt_05to19, codAll)
pointInt_AUD_10to19 <- fn_checkUI(pointInt_10to19, codAll)
pointInt_AUD_15to19 <- fn_checkUI(pointInt_15to19, codAll)
pointInt_AUD_05to14REG <- fn_checkUI(pointInt_05to14REG, codAll, REGIONAL = TRUE)
pointInt_AUD_05to19REG <- fn_checkUI(pointInt_05to19REG, codAll, REGIONAL = TRUE)
pointInt_AUD_10to19REG <- fn_checkUI(pointInt_10to19REG, codAll, REGIONAL = TRUE)
pointInt_AUD_15to19REG <- fn_checkUI(pointInt_15to19REG, codAll, REGIONAL = TRUE)

# Adjust point estimates and uncertainty intervals
pointInt_ADJ_05to14 <- fn_adjustPointIntZeroDeaths(pointInt_05to14, codAll)
pointInt_ADJ_05to19 <- fn_adjustPointIntZeroDeaths(pointInt_05to19, codAll)
pointInt_ADJ_10to19 <- fn_adjustPointIntZeroDeaths(pointInt_10to19, codAll)
pointInt_ADJ_15to19 <- fn_adjustPointIntZeroDeaths(pointInt_15to19, codAll)
pointInt_ADJ_05to14REG <- fn_adjustPointIntZeroDeaths(pointInt_05to14REG, codAll, REGIONAL = TRUE)
pointInt_ADJ_05to19REG <- fn_adjustPointIntZeroDeaths(pointInt_05to19REG, codAll, REGIONAL = TRUE)
pointInt_ADJ_10to19REG <- fn_adjustPointIntZeroDeaths(pointInt_10to19REG, codAll, REGIONAL = TRUE)
pointInt_ADJ_15to19REG <- fn_adjustPointIntZeroDeaths(pointInt_15to19REG, codAll, REGIONAL = TRUE)

# Check if point estimates fall in uncertainty bounds
pointInt_AUD_05to14 <- fn_checkUI(pointInt_ADJ_05to14, codAll)
pointInt_AUD_05to19 <- fn_checkUI(pointInt_05to19, codAll)
pointInt_AUD_10to19 <- fn_checkUI(pointInt_ADJ_10to19, codAll)
pointInt_AUD_15to19 <- fn_checkUI(pointInt_ADJ_15to19, codAll)
pointInt_AUD_05to14REG <- fn_checkUI(pointInt_ADJ_05to14REG, codAll, REGIONAL = TRUE)
pointInt_AUD_05to19REG <- fn_checkUI(pointInt_ADJ_05to19REG, codAll, REGIONAL = TRUE)
pointInt_AUD_10to19REG <- fn_checkUI(pointInt_ADJ_10to19REG, codAll, REGIONAL = TRUE)
pointInt_AUD_15to19REG <- fn_checkUI(pointInt_ADJ_15to19REG, codAll, REGIONAL = TRUE)

# Save csv of all point estimates outside of intervals

if(nrow(pointInt_AUD_05to14) > 0){
  write.csv(pointInt_AUD_05to14, paste("./gen/aggregation/audit/pointInt_AUD_05to14_", aggResDate, ".csv", sep=""), row.names = FALSE)
}
if(nrow(pointInt_AUD_05to19) > 0){
  write.csv(pointInt_AUD_05to14, paste("./gen/aggregation/audit/pointInt_AUD_05to19_", aggResDate, ".csv", sep=""), row.names = FALSE)
}
if(nrow(pointInt_AUD_10to19) > 0){
  write.csv(pointInt_AUD_05to14, paste("./gen/aggregation/audit/pointInt_AUD_10to19_", aggResDate, ".csv", sep=""), row.names = FALSE)
}
if(nrow(pointInt_AUD_15to19) > 0){
  write.csv(pointInt_AUD_05to14, paste("./gen/aggregation/audit/pointInt_AUD_15to19_", aggResDate, ".csv", sep=""), row.names = FALSE)
}

if(nrow(pointInt_AUD_05to14) > 0){
  write.csv(pointInt_AUD_05to14, paste("./gen/aggregation/audit/pointInt_AUD_05to14REG_", aggResDate, ".csv", sep=""), row.names = FALSE)
}
if(nrow(pointInt_AUD_05to19) > 0){
  write.csv(pointInt_AUD_05to14, paste("./gen/aggregation/audit/pointInt_AUD_05to19REG_", aggResDate, ".csv", sep=""), row.names = FALSE)
}
if(nrow(pointInt_AUD_10to19) > 0){
  write.csv(pointInt_AUD_05to14, paste("./gen/aggregation/audit/pointInt_AUD_10to19REG_", aggResDate, ".csv", sep=""), row.names = FALSE)
}
if(nrow(pointInt_AUD_15to19) > 0){
  write.csv(pointInt_AUD_05to14, paste("./gen/aggregation/audit/pointInt_AUD_15to19REG_", aggResDate, ".csv", sep=""), row.names = FALSE)
}

# Save --------------------------------------------------------------------

# Save combination of point estimates and uncertainty intervals
# Note: Typically temporary files don't have a results date. These do because want to keep track of which set of results were aggregated.
# Need to delete old files from this /temp folder every now and then.
write.csv(pointInt_ADJ_05to14, paste("./gen/aggregation/temp/pointInt_05to14_", aggResDate, ".csv", sep = ""), row.names = FALSE)
write.csv(pointInt_ADJ_05to19, paste("./gen/aggregation/temp/pointInt_05to19_", aggResDate, ".csv", sep = ""), row.names = FALSE)
write.csv(pointInt_ADJ_10to19, paste("./gen/aggregation/temp/pointInt_10to19_", aggResDate, ".csv", sep = ""), row.names = FALSE)
write.csv(pointInt_ADJ_15to19, paste("./gen/aggregation/temp/pointInt_15to19_", aggResDate, ".csv", sep = ""), row.names = FALSE)
write.csv(pointInt_ADJ_05to14REG, paste("./gen/aggregation/temp/pointInt_05to14REG_", aggResDate, ".csv", sep=""), row.names = FALSE)
write.csv(pointInt_ADJ_05to19REG, paste("./gen/aggregation/temp/pointInt_05to19REG_", aggResDate, ".csv", sep=""), row.names = FALSE)
write.csv(pointInt_ADJ_10to19REG, paste("./gen/aggregation/temp/pointInt_10to19REG_", aggResDate, ".csv", sep=""), row.names = FALSE)
write.csv(pointInt_ADJ_15to19REG, paste("./gen/aggregation/temp/pointInt_15to19REG_", aggResDate, ".csv", sep=""), row.names = FALSE)

