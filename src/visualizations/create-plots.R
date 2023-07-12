
# Load packages and session variables if not already loaded
if(!exists("sessionVars")){source("./src/load-packages.R")
  load("./gen/data-prep/input/session-variables.Rdata")}

# Load inputs and functions
source("./src/results/results_inputs.R")

# Compare CSMFs for each cause between this round and previous round
plot <- fn_compare_csmf(csmf_Formatted, csmf_OldResults, sample = c("AFG", "BRA", "CHN", "IND", "MEX", "NGA", "SDN"))
ggsave(paste("./gen/results/audit/csmf_comparison_national_", ageGroup,"_", resDate, ".pdf", sep=""), plot, height = 10, width = 8, units = "in")
plot <- fn_compare_csmf(csmf_Formatted_REGION, csmf_OldResults_REGION, regional = TRUE)
ggsave(paste("./gen/results/audit/csmf_comparison_regional_", ageGroup,"_", resDate, ".pdf", sep=""), plot, height = 10, width = 8, units = "in")

# Compare uncertainty intervals for each cause between this round and previous round
###

# Cause-specific rates
### fn_plotrates (add code for country consultation graphs)


# Clear environment of all except session variables
rm(list=setdiff(ls(), sessionVarsList))