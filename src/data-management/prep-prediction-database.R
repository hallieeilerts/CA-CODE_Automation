################################################################################
#' @description Formats prediction database and adds mortality rate covariates so that variable names match covariates used in model estimation.
#' @return Data frame with all covariates required for prediction database
################################################################################
#' Libraries
#' Inputs
source("./src/prepare-session/set-inputs.R")
source("./src/prepare-session/create-session-variables.R")
dat_pred_u20_WHO <- read.csv("./data/prediction-database/PredicationDatabase_2022series_2023-02-21.csv")
env_crisisIncl_u20 <- read.csv("./gen/data-management/output/env_crisisIncl_u20.csv")
key_ctryclass_u20 <- read.csv("./gen/data-management/output/key_ctryclass_u20.csv")
################################################################################

dat <- dat_pred_u20_WHO
env <- env_crisisIncl_u20

names(dat)[names(dat) == "iso3"] <- "ISO3"
names(dat)[names(dat) == "year"] <- "Year"
dat <- dat[dat$Year %in% Years, ]
rownames(dat) <- NULL

## Adjust covariate names

# These are the names that will be saved in the model objects.
# For a simple update, need to make sure the covariates in the updated prediction database match the names in the fitted model objects.
# For a full update, can make minor adjustments to prediction database covariate names to improve clarity, if desired.

names(dat) <- gsub("dtp3", "dtp3_mf", names(dat))
names(dat) <- gsub("underweight", "underwt_mf", names(dat))

# Create contraception_unmet
v_contraception_met <- names(dat)[grep("contraception_met", names(dat))]
v_contraception_met <- v_contraception_met[!(v_contraception_met %in% "contraception_met_source")]
df_contraception_met <- dat[,v_contraception_met]
df_contraception_unmet <- 1 - df_contraception_met
names(df_contraception_unmet) <- gsub("contraception_met", "contraception_unmet", names(df_contraception_unmet))
dat <- cbind(dat, df_contraception_unmet)
dat$contraception_unmet_source <- dat$contraception_met_source

# Move sex suffix to end of sex_age for consistency
v_sex_age <- names(dat)[grep("sex_age", names(dat))]
s_sexSuffix <- gsub("^(?:[^_]+_){2}([^_ ]+)[_].*", "\\1", v_sex_age)
s_ageSuffix <- substr(gsub("^(?:[^_]+_){3}([^_ ]+)", "\\1", v_sex_age), 1, 2)
# Extract text after 4th underscore, recode as blank if only three underscores
s_typeSuffix <- gsub("^(?:[^_]+_){4}([^_ ]+)", "\\1", v_sex_age) 
s_typeSuffix <- ifelse(lengths(regmatches(v_sex_age, gregexpr("_", v_sex_age))) == 3, "", s_typeSuffix)
# Paste together in new order
v_sex_ageAdj <- paste("sex_age", s_ageSuffix, s_sexSuffix, s_typeSuffix, sep = "_")
v_sex_ageAdj <- sub("_$","",v_sex_ageAdj)
names(dat)[grep("sex_age", names(dat))] <- v_sex_ageAdj

## Add mortality rate covariates

# For the 5-19y models, we use some death rates as covariates that are not included in the prediction database.
# These need to be retrieved from the latest IGME envelopes.

# q5to19 both sexes mortality rate used in some HMM models
df_q5to19 <- env[env$AgeLow == 5 & env$AgeUp == 19 & env$Sex == "Both", c("ISO3", "Year", "Rate2")]
names(df_q5to19)[names(df_q5to19) == "Rate2"] <- "q5to19"
dat <- merge(dat, df_q5to19, by = c("ISO3", "Year"), all.x = TRUE)

# q15to19 both sexes mortality rate used in LMM 15-19 males model
df_q15to19 <- env[env$AgeLow == 15 & env$AgeUp == 19 & env$Sex == "Both", c("ISO3", "Year", "Rate2")]
names(df_q15to19)[names(df_q15to19) == "Rate2"] <- "q15to19"
dat <- merge(dat, df_q15to19, by = c("ISO3", "Year"), all.x = TRUE)

# Add lowercase "year" covariate. This is used in some of the LMM models.
dat$year <- dat$Year

# Tidy up
dat <- dat[, c(idVars[1:2], sort(names(dat)[which(!names(dat) %in% idVars[1:2])]))]

# Check that all expected countries are included --------------------------

if(sum(!(unique(key_ctryclass_u20$ISO3) %in% dat$ISO3)) > 0){
  stop("Required countries missing from data input.")
}

# Save output(s) ----------------------------------------------------------

dat_pred_u20 <- dat

write.csv(dat_pred_u20, "./gen/data-management/output/dat_pred_u20.csv", row.names = FALSE)
