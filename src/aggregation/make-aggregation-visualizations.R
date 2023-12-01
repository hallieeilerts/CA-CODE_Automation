
################################################
# Aggregation Visualizations
################################################

# Load inputs and functions
source("./src/prepare-session/prepare-session_master.R") # codAll
source("./src/aggregation/aggregation_inputs.R")
source("./src/aggregation/aggregation_functions.R")

################################################
# Visualizations for intermediate results (while waiting on uncertainty pipeline)
################################################

# Set results date
aggResDate <- "20231016"

# Set sample of countries for plotting
v_sample <- c("AFG", "BRA", "BIH", "CHN",  "HTI", "IND", "IRN", "MEX", "NGA", "SDN", "TUR", "UKR", "YEM")

# Reshape Pancho's aggregate age results file
csmfSqz_PanchoResultsFRMT_10to19 <- fn_reshapePanchoRegAndAgg(csmfSqz_PanchoResults_10to19, codAll)
csmfSqz_PanchoResultsFRMT_10to19REG <- fn_reshapePanchoRegAndAgg(csmfSqz_PanchoResults_10to19REG, codAll)

# Load inputs
# From intermediate results
# csmfSqz_PUB_05to14 <- read.csv(paste("./gen/aggregation/output/PointEstimates_National_05to14_",aggResDate,".csv", sep = ""))
# csmfSqz_PUB_05to19 <- read.csv(paste("./gen/aggregation/output/PointEstimates_National_05to19_",aggResDate,".csv", sep = ""))
# csmfSqz_PUB_10to19 <- read.csv(paste("./gen/aggregation/output/PointEstimates_National_10to19_",aggResDate,".csv", sep = ""))
# csmfSqz_PUB_15to19 <- read.csv(paste("./gen/aggregation/output/PointEstimates_National_15to19_",aggResDate,".csv", sep = ""))
# csmfSqz_PUB_05to14REG <- read.csv(paste("./gen/aggregation/output/PointEstimates_Regional_05to14_",aggResDate,".csv", sep = ""))
# csmfSqz_PUB_05to19REG <- read.csv(paste("./gen/aggregation/output/PointEstimates_Regional_05to19_",aggResDate,".csv", sep = ""))
# csmfSqz_PUB_10to19REG <- read.csv(paste("./gen/aggregation/output/PointEstimates_Regional_10to19_",aggResDate,".csv", sep = ""))
# csmfSqz_PUB_15to19REG <- read.csv(paste("./gen/aggregation/output/PointEstimates_Regional_15to19_",aggResDate,".csv", sep = ""))


# Compare point estimates for each cause between my and Pancho's estimates for this round
plot <- fn_compareCSMF(csmfSqz_PUB_10to19, csmfSqz_PanchoResultsFRMT_10to19, SAMPLE = NULL, AGG = TRUE)
ggsave(paste("./gen/aggregation/audit/csmf_comparison_national_10to19_", resDate, ".pdf", sep=""), plot, height = 10, width = 8, units = "in")
plot <- fn_compareCSMF(csmfSqz_PUB_10to19REG, csmfSqz_PanchoResultsFRMT_10to19REG, REGIONAL = TRUE, AGG = TRUE)
ggsave(paste("./gen/aggregation/audit/csmf_comparison_regional_10to19_", resDate, ".pdf", sep=""), plot, height = 10, width = 8, units = "in")

################################################
# Visualizations for final results
################################################

# Set results date
aggResDate <- "20231201"

# Load inputs
pointInt_PUB_05to14 <- read.csv(paste("./gen/aggregation/output/Uncertainty_National_05to14_",aggResDate,".csv", sep = ""))
pointInt_PUB_05to19 <- read.csv(paste("./gen/aggregation/output/Uncertainty_National_05to19_",aggResDate,".csv", sep = ""))
pointInt_PUB_10to19 <- read.csv(paste("./gen/aggregation/output/Uncertainty_National_10to19_",aggResDate,".csv", sep = ""))
pointInt_PUB_15to19 <- read.csv(paste("./gen/aggregation/output/Uncertainty_National_15to19_",aggResDate,".csv", sep = ""))
pointInt_PUB_05to14REG <- read.csv(paste("./gen/aggregation/output/Uncertainty_Regional_05to14_",aggResDate,".csv", sep = ""))
pointInt_PUB_05to19REG <- read.csv(paste("./gen/aggregation/output/Uncertainty_Regional_05to19_",aggResDate,".csv", sep = ""))
pointInt_PUB_10to19REG <- read.csv(paste("./gen/aggregation/output/Uncertainty_Regional_10to19_",aggResDate,".csv", sep = ""))
pointInt_PUB_15to19REG <- read.csv(paste("./gen/aggregation/output/Uncertainty_Regional_15to19_",aggResDate,".csv", sep = ""))

# Set sample of countries for plotting
v_sample <- c("AFG", "BRA", "BIH", "CHN",  "HTI", "IND", "IRN", "MEX", "NGA", "SDN", "TUR", "UKR", "YEM")

# Plots CSMFs for each cause, no comparison with previous round
# 5-14
plot <- fn_plotSingleCSMF(pointInt_PUB_05to14, SAMPLE = v_sample, VARIABLE = "Fraction")
ggsave(paste("./gen/aggregation/output/csmf_national_05to14_",aggResDate,".pdf", sep=""), plot, height = 10, width = 8, units = "in")
plot <- fn_plotSingleCSMF(pointInt_PUB_05to14REG, VARIABLE = "Fraction", REGIONAL = TRUE)
ggsave(paste("./gen/aggregation/output/csmf_regional_05to14_",aggResDate,".pdf", sep=""), plot, height = 10, width = 8, units = "in")
# 5-19
plot <- fn_plotSingleCSMF(pointInt_PUB_05to19, SAMPLE = v_sample, VARIABLE = "Fraction")
ggsave(paste("./gen/aggregation/output/csmf_national_05to19_",aggResDate,".pdf", sep=""), plot, height = 10, width = 8, units = "in")
plot <- fn_plotSingleCSMF(pointInt_PUB_05to19REG, VARIABLE = "Fraction", REGIONAL = TRUE)
ggsave(paste("./gen/aggregation/output/csmf_regional_05to19_",aggResDate,".pdf", sep=""), plot, height = 10, width = 8, units = "in")
# 10-19
plot <- fn_plotSingleCSMF(pointInt_PUB_10to19, SAMPLE = v_sample, VARIABLE = "Fraction")
ggsave(paste("./gen/aggregation/output/csmf_national_10to19_",aggResDate,".pdf", sep=""), plot, height = 10, width = 8, units = "in")
plot <- fn_plotSingleCSMF(pointInt_PUB_10to19REG, VARIABLE = "Fraction", REGIONAL = TRUE)
ggsave(paste("./gen/aggregation/output/csmf_regional_10to19_",aggResDate,".pdf", sep=""), plot, height = 10, width = 8, units = "in")
# 15-19
plot <- fn_plotSingleCSMF(pointInt_PUB_15to19, SAMPLE = v_sample, VARIABLE = "Fraction")
ggsave(paste("./gen/aggregation/output/csmf_national_15to19_",aggResDate,".pdf", sep=""), plot, height = 10, width = 8, units = "in")
plot <- fn_plotSingleCSMF(pointInt_PUB_15to19REG, VARIABLE = "Fraction", REGIONAL = TRUE)
ggsave(paste("./gen/aggregation/output/csmf_regional_15to19_",aggResDate,".pdf", sep=""), plot, height = 10, width = 8, units = "in")


# Plots UIs for each cause, no comparison with previous round
# 5-14
plot <- fn_plotSingleUI(pointInt_PUB_05to14, codAll, SAMPLE = v_sample, VARIABLE = "Fraction")
ggsave(paste("./gen/aggregation/output/ui_national_05to14_",aggResDate,".pdf", sep=""), plot, height = 10, width = 8, units = "in")
plot <- fn_plotSingleUI(pointInt_PUB_05to14REG, codAll, VARIABLE = "Fraction", REGIONAL = TRUE)
ggsave(paste("./gen/aggregation/output/ui_regional_05to14_",aggResDate,".pdf", sep=""), plot, height = 10, width = 8, units = "in")
# 5-19
plot <- fn_plotSingleUI(pointInt_PUB_05to19, codAll, SAMPLE = v_sample, VARIABLE = "Fraction")
ggsave(paste("./gen/aggregation/output/ui_national_05to19_",aggResDate,".pdf", sep=""), plot, height = 10, width = 8, units = "in")
plot <- fn_plotSingleUI(pointInt_PUB_05to19REG, codAll, VARIABLE = "Fraction", REGIONAL = TRUE)
ggsave(paste("./gen/aggregation/output/ui_regional_05to19_",aggResDate,".pdf", sep=""), plot, height = 10, width = 8, units = "in")
# 10-19
plot <- fn_plotSingleUI(pointInt_PUB_10to19, codAll, SAMPLE = v_sample, VARIABLE = "Fraction")
ggsave(paste("./gen/aggregation/output/ui_national_10to19_",aggResDate,".pdf", sep=""), plot, height = 10, width = 8, units = "in")
plot <- fn_plotSingleUI(pointInt_PUB_10to19REG, codAll, VARIABLE = "Fraction", REGIONAL = TRUE)
ggsave(paste("./gen/aggregation/output/ui_regional_10to19_",aggResDate,".pdf", sep=""), plot, height = 10, width = 8, units = "in")
# 15-19
plot <- fn_plotSingleUI(pointInt_PUB_15to19, codAll, SAMPLE = v_sample, VARIABLE = "Fraction")
ggsave(paste("./gen/aggregation/output/ui_national_15to19_",aggResDate,".pdf", sep=""), plot, height = 10, width = 8, units = "in")
plot <- fn_plotSingleUI(pointInt_PUB_15to19REG, codAll, VARIABLE = "Fraction", REGIONAL = TRUE)
ggsave(paste("./gen/aggregation/output/ui_regional_15to19_",aggResDate,".pdf", sep=""), plot, height = 10, width = 8, units = "in")


