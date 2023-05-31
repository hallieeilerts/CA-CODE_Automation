###################################################################
########################## BEGIN-INPUTS ###########################
###################################################################

# Load packages and session variables if not already loaded
if(!exists("sessionVars")){source("./src/prepare-session.R")
  load("./gen/data-prep/input/session-variables.Rdata")}

# Load input(s)
db_pred_u20_who <- read.csv("./data/prediction-database/PredicationDatabase_2022series_2023-02-21.csv")
env_crisisincl_u20 <- read.csv("./gen/data-prep/output/env_crisisincl_u20.csv")

###################################################################
########################## END-INPUTS #############################
###################################################################

dat <- db_pred_u20_who

names(dat)[names(dat) == "iso3"] <- "ISO3"
names(dat)[names(dat) == "year"] <- "Year"
dat <- dat[dat$Year %in% Years, ]
rownames(dat) <- NULL

#--------------------#
# 2021 SIMPLE UPDATE #
#--------------------#

# ADJUST COVARIATE NAMES
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

rm(v_contraception_met, df_contraception_met, df_contraception_unmet)

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

rm(v_sex_age, v_sex_ageAdj, s_sexSuffix, s_ageSuffix, s_typeSuffix)

# ADD COVARIATES

# For the 5-19y models, we use some death rates as covariates that are not included in the prediction database.
# These need to be retrieved from the latest IGME envelopes.
df_q5to19 <- env_crisisincl_u20[env_crisisincl_u20$AgeLow == 5 & env_crisisincl_u20$AgeUp == 19, c("ISO3", "Year", "Rate2")]
names(df_q5to19)[names(df_q5to19) == "Rate2"] <- "q5to19"
dat <- merge(dat, df_q5to19, by = c("ISO3", "Year"), all.x = TRUE)

# 5q15 both sexes mortality rate used in LMM 15-19 males model
df_q15to19 <- env_crisisincl_u20[env_crisisincl_u20$AgeLow == 15 & env_crisisincl_u20$AgeUp == 19 & env_crisisincl_u20$Sex == "B", c("ISO3", "Year", "Rate2")]
names(df_q15to19)[names(df_q15to19) == "Rate2"] <- "q15to19"
dat <- merge(dat, df_q15to19, by = c("ISO3", "Year"), all.x = TRUE)

# Remove unnecessary objects
rm(df_q5to19, df_q15to19)

# Add lowercase "year" covariate. This is used in some of the LMM models.
dat$year <- dat$Year

# Tidy up
dat <- dat[, c(idVars[1:2], sort(names(dat)[which(!names(dat) %in% idVars[1:2])]))]

# Remove unnecessary objects
rm(db_pred_u20_who, env_crisisincl_u20)

###################################################################
######################### BEGIN-OUTPUTS ###########################
###################################################################

# Save output(s)
write.csv(dat, "./gen/data-prep/output/db_pred_u20.csv", row.names = FALSE)

###################################################################
######################### END-OUTPUTS #############################
###################################################################
