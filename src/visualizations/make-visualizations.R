################################################
# Visualizations
################################################

# Load inputs and functions
source("./src/visualizations/visualizations_inputs.R")
source("./src/visualizations/visualizations_functions.R")

v_sample <- c("AFG", "BRA", "BIH", "CHN",  "HTI", "IND", "IRN", "MEX", "NGA", "SDN", "TUR", "UKR", "YEM")

## Audit

# Reshape Pancho's new regional results files, if need be
point_PanchoResultsFRMT_REG <- fn_reshapePanchoRegional(point_PanchoResults_REG)

# Compare point estimates for each cause between my and Pancho's estimates for this round
plot <- fn_compareCSMF(point, point_PanchoResults, SAMPLE = NULL)
ggsave(paste("./gen/visualizations/audit/csmf_comparison_national_", ageGroup,"_", resDate, ".pdf", sep=""), plot, height = 10, width = 8, units = "in")
plot <- fn_compareCSMF(point_REG, point_PanchoResultsFRMT_REG, REGIONAL = TRUE)
ggsave(paste("./gen/visualizations/audit/csmf_comparison_regional_", ageGroup,"_", resDate, ".pdf", sep=""), plot, height = 10, width = 8, units = "in")

# Compare uncertainty intervals for each cause between my and Pancho's estimates for this round
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

## Output

# Compare CSMFs for each cause between this round and previous round
plot <- fn_compareCSMF(point, point_PrevResults, sample = v_sample)
ggsave(paste("./gen/visualizations/output/csmf_comparison_national_", ageGroup,"_", resDate, ".pdf", sep=""), plot, height = 10, width = 8, units = "in")
plot <- fn_compareCSMF(point_REG, point_prevResults_REGIONAL, regional = TRUE)
ggsave(paste("./gen/visualizations/output/csmf_comparison_regional_", ageGroup,"_", resDate, ".pdf", sep=""), plot, height = 10, width = 8, units = "in")


# Cause-specific rates
### fn_plotrates (add code for country consultation graphs)

