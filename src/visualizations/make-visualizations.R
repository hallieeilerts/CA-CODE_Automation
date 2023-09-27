################################################
# Visualizations
################################################

# Load inputs and functions
source("./src/visualizations/visualizations_inputs.R")
source("./src/visualizations/visualizations_functions.R")

v_sample <- c("AFG", "BRA", "BIH", "CHN",  "HTI", "IND", "IRN", "MEX", "NGA", "SDN", "TUR", "UKR", "YEM")

## Audit
# Compare CSMFs for each cause between my and Pancho's estimates for this round
plot <- fn_compareCSMF(point, point_PanchoResults, SAMPLE = v_sample)
ggsave(paste("./gen/visualizations/audit/csmf_comparisonPANCHO_national_", ageGroup,"_", resDate, ".pdf", sep=""), plot, height = 10, width = 8, units = "in")
plot <- fn_compareCSMF(point_REG, point_PanchoResults_REG, REGIONAL = TRUE)
ggsave(paste("./gen/visualizations/audit/csmf_comparisonPANCHO_regional_", ageGroup,"_", resDate, ".pdf", sep=""), plot, height = 10, width = 8, units = "in")
# Compare uncertainty intervals for each cause between my and Pancho's estimates for this round
plot <- fn_compareUI(pointInt, pointInt_PanchoResults, VARIABLE = "Fraction", CODALL = codAll, SAMPLE = v_sample)
ggsave(paste("./gen/visualizations/audit/ui_comparisonPANCHO_national_", ageGroup,"_", resDate, ".pdf", sep=""), plot, height = 10, width = 8, units = "in")
plot <- fn_compareUI(pointInt_REG, point_PanchoResults_REG, VARIABLE = "Fraction", CODALL = codAll, REGIONAL = TRUE)
ggsave(paste("./gen/visualizations/audit/ui_comparisonPANCHO_regional_", ageGroup,"_", resDate, ".pdf", sep=""), plot, height = 10, width = 8, units = "in")

## Output
# Compare CSMFs for each cause between this round and previous round
plot <- fn_compareCSMF(point, point_PrevResults, sample = v_sample)
ggsave(paste("./gen/visualizations/output/csmf_comparisonPREVROUND_national_", ageGroup,"_", resDate, ".pdf", sep=""), plot, height = 10, width = 8, units = "in")
plot <- fn_compareCSMF(point_REG, point_prevResults_REGIONAL, regional = TRUE)
ggsave(paste("./gen/visualizations/output/csmf_comparisonPREVROUND_regional_", ageGroup,"_", resDate, ".pdf", sep=""), plot, height = 10, width = 8, units = "in")


# Cause-specific rates
### fn_plotrates (add code for country consultation graphs)


