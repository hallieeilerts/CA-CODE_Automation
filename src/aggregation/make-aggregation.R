################################################
# Aggregation
################################################

# Load inputs and functions
source("./src/aggregation/aggregation_inputs.R")
source("./src/aggregation/aggregation_functions.R")
source("./src/squeezing/fn_calcRegion.R")

# Calculate CSMFs for aggregate age group (need to have already produced CSMFs for all standard age groups)
csmfSqz_05to14 <- fn_calcAggAges(AGELB = 5, AGEUB = 14, CODALL = codAll, CSMF_5TO9 = csmfSqz_05to09,  CSMF_10TO14 = csmfSqz_10to14)
csmfSqz_05to19 <- fn_calcAggAges(AGELB = 5, AGEUB = 19, CODALL = codAll, CSMF_5TO9 = csmfSqz_05to09,  CSMF_10TO14 = csmfSqz_10to14, CSMF_15TO19F = csmfSqz_15to19f, CSMF_15TO19M = csmfSqz_15to19m, ENV = env_u20)
csmfSqz_10to19 <- fn_calcAggAges(AGELB = 10, AGEUB = 19, CODALL = codAll, CSMF_10TO14 = csmfSqz_10to14, CSMF_15TO19F = csmfSqz_15to19f, CSMF_15TO19M = csmfSqz_15to19m, ENV = env_u20)

# Save aggregated point estimates
write.csv(csmfSqz_05to14, paste("./gen/aggregation/temp/csmfSqz_05to14.csv", sep = ""))
write.csv(csmfSqz_05to19, paste("./gen/aggregation/temp/csmfSqz_05to19.csv", sep = ""))
write.csv(csmfSqz_10to19, paste("./gen/aggregation/temp/csmfSqz_10to19.csv", sep = ""))

# Sample from envelope draws for 15-19 sexes combined
envDraws_SAMP_15to19 <- fn_rand_draw_env(envDraws_15to19, v_sample$env)
rm(envDraws_15to19)

# Calculate CSMF draws for aggregate age groups
csmfSqzDraws_05to14 <- fn_callCalcAggAges(AGELB=5, AGEUB=14, CODALL=codAll, 
                                             CSMF_5TO9 = csmfSqzDraws_05to09, 
                                             CSMF_10TO14 = csmfSqzDraws_10to14, 
                                             CSMF_15TO19F = csmfSqzDraws_15to19f, CSMF_15TO19M = csmfSqzDraws_15to19m, 
                                             ENV = envDraws_SAMP_15to19, UNCERTAINTY = TRUE)
csmfSqzDraws_05to19 <- fn_callCalcAggAges(AGELB=5, AGEUB=19, CODALL=codAll, 
                                             CSMF_5TO9 = csmfSqzDraws_05to09, 
                                             CSMF_10TO14 = csmfSqzDraws_10to14, 
                                             CSMF_15TO19F = csmfSqzDraws_15to19f, CSMF_15TO19M = csmfSqzDraws_15to19m, 
                                             ENV = envDraws_SAMP_15to19, UNCERTAINTY = TRUE)
csmfSqzDraws_10to19 <- fn_callCalcAggAges(AGELB=10, AGEUB=19, CODALL=codAll,
                                             CSMF_5TO9 = csmfSqzDraws_05to09, 
                                             CSMF_10TO14 = csmfSqzDraws_10to14, 
                                             CSMF_15TO19F = csmfSqzDraws_15to19f, CSMF_15TO19M = csmfSqzDraws_15to19m, 
                                             ENV = envDraws_SAMP_15to19, UNCERTAINTY = TRUE)

# Calculate regional CSMFs
csmfSqzDraws_05to14REG <- lapply(csmfSqzDraws_05to14, function(x){ fn_calcRegion(x, codAll, key_region) })
csmfSqzDraws_05to19REG <- lapply(csmfSqzDraws_05to14, function(x){ fn_calcRegion(x, codAll, key_region) })
csmfSqzDraws_10to19REG <- lapply(csmfSqzDraws_05to14, function(x){ fn_calcRegion(x, codAll, key_region) })

################################################
# Uncertainty
################################################

# Load inputs and functions
source("./src/uncertainty/uncertainty_functions.R")

# Calculate uncertainty intervals
ui_05to14 <- fn_calc_ui(csmfSqzDraws_05to14, UI = 0.95, CODALL = codAll)
ui_05to19 <- fn_calc_ui(csmfSqzDraws_05to19, UI = 0.95, CODALL = codAll)
ui_10to19 <- fn_calc_ui(csmfSqzDraws_10to19, UI = 0.95, CODALL = codAll)
ui_05to14REG <- fn_calc_ui(csmfSqzDraws_05to14REG, UI = 0.95, CODALL = codAll, REGIONAL = TRUE)
ui_05to19REG <- fn_calc_ui(csmfSqzDraws_05to19REG, UI = 0.95, CODALL = codAll, REGIONAL = TRUE)
ui_10to19REG <- fn_calc_ui(csmfSqzDraws_10to19REG, UI = 0.95, CODALL = codAll, REGIONAL = TRUE)

# Round median estimates with uncertainty intervals
pointInt_FRMT_05to14 <- fn_round_pointint(ui_05to14, codAll)
pointInt_FRMT_05to19 <- fn_round_pointint(ui_05to19, codAll)
pointInt_FRMT_10to19 <- fn_round_pointint(ui_10to19, codAll)
pointInt_FRMT_05to14REG <- fn_round_pointint(ui_05to14REG, codAll, REGIONAL = TRUE)
pointInt_FRMT_05to19REG <- fn_round_pointint(ui_05to19REG, codAll, REGIONAL = TRUE)
pointInt_FRMT_10to19REG <- fn_round_pointint(ui_10to19REG, codAll, REGIONAL = TRUE)

# Audit: confirm that median estimates fall in uncertainty bounds
pointInt_AUD_05to14 <- fn_check_ui(pointInt_FRMT_05to14, codAll, QUANTILE = "median")
pointInt_AUD_05to19 <- fn_check_ui(pointInt_FRMT_05to19, codAll, QUANTILE = "median")
pointInt_AUD_10to19 <- fn_check_ui(pointInt_FRMT_10to19, codAll, QUANTILE = "median")
pointInt_AUD_05to14REG <- fn_check_ui(pointInt_FRMT_05to14REG, codAll, QUANTILE = "median", REGIONAL = TRUE)
pointInt_AUD_05to19REG <- fn_check_ui(pointInt_FRMT_05to19REG, codAll, QUANTILE = "median", REGIONAL = TRUE)
pointInt_AUD_10to19REG <- fn_check_ui(pointInt_FRMT_10to19REG, codAll, QUANTILE = "median", REGIONAL = TRUE)
sum(unlist(lapply(list(pointInt_AUD_05to14, pointInt_AUD_05to19, pointInt_AUD_10to19, pointInt_AUD_05to14REG, pointInt_AUD_05to19REG, pointInt_AUD_10to19REG), nrow)))

# Add age columns
pointInt_FRMT_05to14 <- fn_addAgeCol(pointInt_FRMT_05to14, 5, 14)
pointInt_FRMT_05to19 <- fn_addAgeCol(pointInt_FRMT_05to19, 5, 19)
pointInt_FRMT_10to19 <- fn_addAgeCol(pointInt_FRMT_05to14, 10, 19)
pointInt_FRMT_05to14REG <- fn_addAgeCol(pointInt_FRMT_05to14REG, 5, 14)
pointInt_FRMT_05to19REG <- fn_addAgeCol(pointInt_FRMT_05to19REG, 5, 19)
pointInt_FRMT_10to19REG <- fn_addAgeCol(pointInt_FRMT_05to14REG, 10, 19)

# Save
write.csv(pointInt_FRMT_05to14, paste("./gen/aggregation/temp/pointInt_05to14.csv", sep=""), row.names = FALSE)
write.csv(pointInt_FRMT_05to19, paste("./gen/aggregation/temp/pointInt_05to19.csv", sep=""), row.names = FALSE)
write.csv(pointInt_FRMT_10to19, paste("./gen/aggregation/temp/pointInt_10to19.csv", sep=""), row.names = FALSE)
write.csv(pointInt_FRMT_05to14REG, paste("./gen/aggregation/temp/pointInt_05to14REG.csv", sep=""), row.names = FALSE)
write.csv(pointInt_FRMT_05to19REG, paste("./gen/aggregation/temp/pointInt_05to19REG.csv", sep=""), row.names = FALSE)
write.csv(pointInt_FRMT_10to19REG, paste("./gen/aggregation/temp/pointInt_10to19REG.csv", sep=""), row.names = FALSE)

################################################
# Results
################################################

# Load inputs and functions
# pointInt_FMT_05to14 <- read.csv(paste("./gen/aggregation/temp/pointInt_05to14.csv", sep = ""))
# pointInt_FMT_05to19 <- read.csv(paste("./gen/aggregation/temp/pointInt_05to19.csv", sep = ""))
# pointInt_FMT_10to19 <- read.csv(paste("./gen/aggregation/temp/pointInt_10to19.csv", sep = ""))
# pointInt_FMT_05to14REG <- read.csv(paste("./gen/aggregation/temp/pointInt_05to14REG.csv", sep = ""))
# pointInt_FMT_05to19REG <- read.csv(paste("./gen/aggregation/temp/pointInt_05to19REG.csv", sep = ""))
# pointInt_FMT_10to19REG <- read.csv(paste("./gen/aggregation/temp/pointInt_10to19REG.csv", sep = ""))
source("./src/results/results_functions.R")

# Finalize estimates from uncertainty pipeline (final results)
point_PUB_05to14 <- fn_publish_estimates(pointInt_FRMT_05to14, key_region, key_ctryclass, codAll, UNCERTAINTY = FALSE)
point_PUB_05to19 <- fn_publish_estimates(pointInt_FRMT_05to19, key_region, key_ctryclass, codAll, UNCERTAINTY = FALSE)
point_PUB_10to19 <- fn_publish_estimates(pointInt_FRMT_10to19, key_region, key_ctryclass, codAll, UNCERTAINTY = FALSE)
point_PUB_05to14REG <- fn_publish_estimates(pointInt_FRMT_05to14REG, key_region, key_ctryclass, codAll, UNCERTAINTY = FALSE, REGIONAL = TRUE)
point_PUB_05to19REG <- fn_publish_estimates(pointInt_FRMT_05to19REG, key_region, key_ctryclass, codAll, UNCERTAINTY = FALSE, REGIONAL = TRUE)
point_PUB_10to19REG <- fn_publish_estimates(pointInt_FRMT_10to19REG, key_region, key_ctryclass, codAll, UNCERTAINTY = FALSE, REGIONAL = TRUE)
pointInt_PUB_05to14 <- fn_publish_estimates(pointInt_FRMT_05to14, key_region, key_ctryclass, codAll, UNCERTAINTY = TRUE)
pointInt_PUB_05to19 <- fn_publish_estimates(pointInt_FRMT_05to19, key_region, key_ctryclass, codAll, UNCERTAINTY = TRUE)
pointInt_PUB_10to19 <- fn_publish_estimates(pointInt_FRMT_10to19, key_region, key_ctryclass, codAll, UNCERTAINTY = TRUE)
pointInt_PUB_05to14REG <- fn_publish_estimates(pointInt_FRMT_05to14REG, key_region, key_ctryclass, codAll, UNCERTAINTY = TRUE, REGIONAL = TRUE)
pointInt_PUB_05to19REG <- fn_publish_estimates(pointInt_FRMT_05to19REG, key_region, key_ctryclass, codAll, UNCERTAINTY = TRUE, REGIONAL = TRUE)
pointInt_PUB_10to19REG <- fn_publish_estimates(pointInt_FRMT_10to19REG, key_region, key_ctryclass, codAll, UNCERTAINTY = TRUE, REGIONAL = TRUE)

# Save final versions
write.csv(point_PUB_05to14, paste("./gen/aggregation/output/PointEstimates_National_05to14_", resDate, ".csv", sep=""), row.names = FALSE)
write.csv(point_PUB_05to19, paste("./gen/aggregation/output/PointEstimates_National_05to19_", resDate, ".csv", sep=""), row.names = FALSE)
write.csv(point_PUB_10to19, paste("./gen/aggregation/output/PointEstimates_National_10to19_", resDate, ".csv", sep=""), row.names = FALSE)
write.csv(point_PUB_05to14REG, paste("./gen/aggregation/output/PointEstimates_Regional_05to14_", resDate, ".csv", sep=""), row.names = FALSE)
write.csv(point_PUB_05to19REG, paste("./gen/aggregation/output/PointEstimates_Regional_05to19_", resDate, ".csv", sep=""), row.names = FALSE)
write.csv(point_PUB_10to19REG, paste("./gen/aggregation/output/PointEstimates_Regional_10to19_", resDate, ".csv", sep=""), row.names = FALSE)
write.csv(pointInt_PUB_05to14, paste("./gen/aggregation/output/Uncertainty_National_05to14_", resDate, ".csv", sep=""), row.names = FALSE)
write.csv(pointInt_PUB_05to19, paste("./gen/aggregation/output/Uncertainty_National_05to19_", resDate, ".csv", sep=""), row.names = FALSE)
write.csv(pointInt_PUB_10to19, paste("./gen/aggregation/output/Uncertainty_National_10to19_", resDate, ".csv", sep=""), row.names = FALSE)
write.csv(pointInt_PUB_05to14REG, paste("./gen/aggregation/output/Uncertainty_Regional_05to14_", resDate, ".csv", sep=""), row.names = FALSE)
write.csv(pointInt_PUB_05to19REG, paste("./gen/aggregation/output/Uncertainty_Regional_05to19_", resDate, ".csv", sep=""), row.names = FALSE)
write.csv(pointInt_PUB_10to19REG, paste("./gen/aggregation/output/Uncertainty_Regional_10to19_", resDate, ".csv", sep=""), row.names = FALSE)

################################################
# Visualizations
################################################

# Load inputs and functions
# Set results date
# aggResDate <- "20230925"
# pointInt_PUB_05to14 <- read.csv(paste("./gen/aggregation/output/Uncertainty_National_05to14_",aggResDate,".csv", sep = ""))
# pointInt_PUB_05to19 <- read.csv(paste("./gen/aggregation/output/Uncertainty_National_05to19_",aggResDate,".csv", sep = ""))
# pointInt_PUB_10to19 <- read.csv(paste("./gen/aggregation/output/Uncertainty_National_10to19_",aggResDate,".csv", sep = ""))
# pointInt_PUB_05to14REG <- read.csv(paste("./gen/aggregation/output/Uncertainty_Regional_05to14_",aggResDate,".csv", sep = ""))
# pointInt_PUB_05to19REG <- read.csv(paste("./gen/aggregation/output/Uncertainty_Regional_05to19_",aggResDate,".csv", sep = ""))
# pointInt_PUB_10to19REG <- read.csv(paste("./gen/aggregation/output/Uncertainty_Regional_10to19_",aggResDate,".csv", sep = ""))
source("./src/visualizations/visualizations_functions.R")

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

# Plots UIs for each cause, no comparison with previous round
# 5-14
plot <- fn_plotSingleUI(pointInt_PUB_05to14, codAll, SAMPLE = v_sample, VARIABLE = "Fraction")
ggsave(paste("./gen/aggregation/output/ui_national_05to14_",aggResDate,".pdf", sep=""), plot, height = 10, width = 8, units = "in")
plot <- fn_plotSingleUI(pointInt_05to14REG, codAll, VARIABLE = "Fraction", REGIONAL = TRUE)
ggsave(paste("./gen/aggregation/output/ui_regional_05to14_",aggResDate,".pdf", sep=""), plot, height = 10, width = 8, units = "in")
# 5-19
plot <- fn_plotSingleUI(pointInt_PUB_05to19, codAll, SAMPLE = v_sample, VARIABLE = "Fraction")
ggsave(paste("./gen/aggregation/output/ui_national_05to19_",aggResDate,".pdf", sep=""), plot, height = 10, width = 8, units = "in")
plot <- fn_plotSingleUI(pointInt_05to19REG, codAll, VARIABLE = "Fraction", REGIONAL = TRUE)
ggsave(paste("./gen/aggregation/output/ui_regional_05to19_",aggResDate,".pdf", sep=""), plot, height = 10, width = 8, units = "in")
# 10-19
plot <- fn_plotSingleUI(pointInt_PUB_10to19, codAll, SAMPLE = v_sample, VARIABLE = "Fraction")
ggsave(paste("./gen/aggregation/output/ui_national_10to19_",aggResDate,".pdf", sep=""), plot, height = 10, width = 8, units = "in")
plot <- fn_plotSingleUI(pointInt_10to19REG, codAll, VARIABLE = "Fraction", REGIONAL = TRUE)
ggsave(paste("./gen/aggregation/output/ui_regional_10to19_",aggResDate,".pdf", sep=""), plot, height = 10, width = 8, units = "in")




