################################################
# Visualizations
################################################

# Clear environment
rm(list = ls())

# Load inputs and functions
source("./src/visualizations/visualizations_inputs.R")
source("./src/visualizations/visualizations_functions.R")

v_sample <- c("AFG", "BRA", "BIH", "CHN",  "HTI", "IND", "IRN", "MEX", "NGA", "SDN", "TUR", "UKR", "YEM")

#### Audit

# Compare between my and Pancho's estimates for this round
# I have restructured his code, but these should match almost exactly.

# Reshape Pancho's regional results file
point_PanchoResultsFRMT_REG <- fn_reshapePanchoRegAndAgg(point_PanchoResults_REG, codAll)

# Point estimates
plot <- fn_compareCSMF(point, point_PanchoResults, SAMPLE = NULL)
ggsave(paste("./gen/visualizations/audit/csmf_comparison_national_", ageGroup,"_", resDate, ".pdf", sep=""), plot, height = 10, width = 8, units = "in")
plot <- fn_compareCSMF(point_REG, point_PanchoResultsFRMT_REG, REGIONAL = TRUE)
ggsave(paste("./gen/visualizations/audit/csmf_comparison_regional_", ageGroup,"_", resDate, ".pdf", sep=""), plot, height = 10, width = 8, units = "in")

# Uncertainty intervals
plot <- fn_compareUI(pointInt, pointInt_PanchoResults, VARIABLE = "Fraction", CODALL = codAll, SAMPLE = NULL)
ggsave(paste("./gen/visualizations/audit/ui_comparison_national_", ageGroup,"_", resDate, ".pdf", sep=""), plot, height = 10, width = 8, units = "in")
plot <- fn_compareUI(pointInt_REG, point_PanchoResultsFRMT_REG, VARIABLE = "Fraction", CODALL = codAll, REGIONAL = TRUE)
ggsave(paste("./gen/visualizations/audit/ui_comparison_regional_", ageGroup,"_", resDate, ".pdf", sep=""), plot, height = 10, width = 8, units = "in")
plot <- fn_compareUIfixY(pointInt_REG, point_PanchoResultsFRMT_REG, VARIABLE = "Fraction", CODALL = codAll, REGIONAL = TRUE)
ggsave(paste("./gen/visualizations/audit/ui_comparison_regional_", ageGroup,"_", resDate, "fixedY.pdf", sep=""), plot, height = 10, width = 8, units = "in")
#' Note: My regional point estimates and pancho's differ slightly.
#' This is only visible in the UI plots because they're so zoomed in.
#' It is only a difference of rounding
#' I calculate the national estimates, aggregate to regional, and everything is rounded at the end.
#' Pancho's regional point estimates were calculated from the rounded national estimates in RegionalPointEstimates.R

#### Output

# Compare between my estimates for this round and those of previous round

# Compare CSMFs for each cause between this round and previous round
plot <- fn_compareCSMFjamieStyle(pointInt, pointInt_REG, point_PrevResults, point_PrevResults_REG)
ggsave(paste("./gen/visualizations/output/Compare_estimates_", ageGroup,"_", resDate, ".pdf", sep=""), plot, height = 10, width = 8, units = "in")


