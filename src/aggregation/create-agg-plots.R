################################################################################
#' @description Creates plots for CSMFs and uncertainty for aggregate age groups
#' @return PDFs with plots for aggregate age groups
################################################################################

# Set
aggResDate <- "20230925"
v_sample <- c("AFG", "BRA", "BIH", "CHN",  "HTI", "IND", "IRN", "MEX", "NGA", "SDN", "TUR", "UKR", "YEM")

# Load data
# 5-14
pointInt_05to14 <- read.csv(paste("./gen/aggregation/output/Uncertainty_National_05to14_",aggResDate,".csv", sep = ""))
pointInt_05to14REG <- read.csv(paste("./gen/aggregation/output/Uncertainty_Regional_05to14_",aggResDate,".csv", sep = ""))
# 5-19
pointInt_05to19 <- read.csv(paste("./gen/aggregation/output/Uncertainty_National_05to19_",aggResDate,".csv", sep = ""))
pointInt_05to19REG <- read.csv(paste("./gen/aggregation/output/Uncertainty_Regional_05to19_",aggResDate,".csv", sep = ""))
# 10-19
pointInt_10to19 <- read.csv(paste("./gen/aggregation/output/Uncertainty_National_10to19_",aggResDate,".csv", sep = ""))
pointInt_10to19REG <- read.csv(paste("./gen/aggregation/output/Uncertainty_Regional_10to19_",aggResDate,".csv", sep = ""))

# Plots CSMFs for each cause, no comparison with previous round
# 5-14
plot <- fn_plot_single_csmf(pointInt_05to14, SAMPLE = v_sample, VARIABLE = "Fraction")
ggsave(paste("./gen/aggregation/output/csmf_national_05to14_",aggResDate,".pdf", sep=""), plot, height = 10, width = 8, units = "in")
plot <- fn_plot_single_csmf(pointInt_05to14REG, VARIABLE = "Fraction", REGIONAL = TRUE)
ggsave(paste("./gen/aggregation/output/csmf_regional_05to14_",aggResDate,".pdf", sep=""), plot, height = 10, width = 8, units = "in")
# 5-19
plot <- fn_plot_single_csmf(pointInt_05to19, SAMPLE = v_sample, VARIABLE = "Fraction")
ggsave(paste("./gen/aggregation/output/csmf_national_05to19_",aggResDate,".pdf", sep=""), plot, height = 10, width = 8, units = "in")
plot <- fn_plot_single_csmf(pointInt_05to19REG, VARIABLE = "Fraction", REGIONAL = TRUE)
ggsave(paste("./gen/aggregation/output/csmf_regional_05to19_",aggResDate,".pdf", sep=""), plot, height = 10, width = 8, units = "in")
# 10-19
plot <- fn_plot_single_csmf(pointInt_10to19, SAMPLE = v_sample, VARIABLE = "Fraction")
ggsave(paste("./gen/aggregation/output/csmf_national_10to19_",aggResDate,".pdf", sep=""), plot, height = 10, width = 8, units = "in")
plot <- fn_plot_single_csmf(pointInt_10to19REG, VARIABLE = "Fraction", REGIONAL = TRUE)
ggsave(paste("./gen/aggregation/output/csmf_regional_10to19_",aggResDate,".pdf", sep=""), plot, height = 10, width = 8, units = "in")

# Plots UIs for each cause, no comparison with previous round
# 5-14
plot <- fn_plot_single_ui(pointInt_05to14, codAll, SAMPLE = v_sample, VARIABLE = "Fraction")
ggsave(paste("./gen/aggregation/output/ui_national_05to14_",aggResDate,".pdf", sep=""), plot, height = 10, width = 8, units = "in")
plot <- fn_plot_single_ui(pointInt_05to14REG, codAll, VARIABLE = "Fraction", REGIONAL = TRUE)
ggsave(paste("./gen/aggregation/output/ui_regional_05to14_",aggResDate,".pdf", sep=""), plot, height = 10, width = 8, units = "in")
# 5-19
plot <- fn_plot_single_ui(pointInt_05to19, codAll, SAMPLE = v_sample, VARIABLE = "Fraction")
ggsave(paste("./gen/aggregation/output/ui_national_05to19_",aggResDate,".pdf", sep=""), plot, height = 10, width = 8, units = "in")
plot <- fn_plot_single_ui(pointInt_05to19REG, codAll, VARIABLE = "Fraction", REGIONAL = TRUE)
ggsave(paste("./gen/aggregation/output/ui_regional_05to19_",aggResDate,".pdf", sep=""), plot, height = 10, width = 8, units = "in")
# 10-19
plot <- fn_plot_single_ui(pointInt_10to19, codAll, SAMPLE = v_sample, VARIABLE = "Fraction")
ggsave(paste("./gen/aggregation/output/ui_national_10to19_",aggResDate,".pdf", sep=""), plot, height = 10, width = 8, units = "in")
plot <- fn_plot_single_ui(pointInt_10to19REG, codAll, VARIABLE = "Fraction", REGIONAL = TRUE)
ggsave(paste("./gen/aggregation/output/ui_regional_10to19_",aggResDate,".pdf", sep=""), plot, height = 10, width = 8, units = "in")


