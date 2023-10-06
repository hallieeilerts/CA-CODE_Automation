################################################################################
#' @description Rename columns
#' @return Data frame with c("ISO3", "Year", "dth_malaria_5to19")
################################################################################
#' Libraries
require(readstata13)
#' Inputs
source("./src/prepare-session/set-inputs.R")
source("./src/prepare-session/create-session-variables.R")
csmf_01to04 <- read.dta13("./data/mortality-fractions/child_cod_2000-2021.dta")
key_ctryclass_u20 <- read.csv("./gen/data-management/output/key_ctryclass_u20.csv")
################################################################################

dat <- csmf_01to04

# Select countries (exclude regions)
dat <- subset(dat, !is.na(iso3))
dat <- subset(dat, iso3 != "NA")
# Refer to "child_cod_2000-2021.xls" readme tab to identify column name for postneonatal malaria CSMF
dat$csmf_malaria_01to04 <- dat$post8/dat$pnd
dat$csmf_malaria_01to04[is.na(dat$csmf_malaria_01to04)] <- 0
dat <- dat[,c("iso3", "year", "csmf_malaria_01to04")]
names(dat) <- c("ISO3", "Year", "csmf_malaria_01to04")

# Check that all expected countries are included --------------------------

if(sum(!(unique(key_ctryclass_u20$ISO3) %in% dat$ISO3)) > 0){
  warning("Not all countries included in data input.")
  write.table(sort(unique(key_ctryclass_u20$WHOname)[!(unique(key_ctryclass_u20$ISO3) %in% dat$ISO3)]), 
              "./gen/data-management/audit/missing_mal-frac-cap.txt")
}

# Fill in zeros for missing country-years, if necessary

# Create data frame for countries/years of interest
# For malaria fraction cap data, HMM countries
df_ctryyears <- data.frame(ISO3 = rep(key_ctryclass_u20$ISO3, each = length(Years)),
                           Year = rep(Years))

# Merge onto COD data, identifying missing countries/years
dat <- merge(dat, df_ctryyears, by = idVars[1:2], all = TRUE)

# Recode missing csmf_malaria_01to04 as 0
dat$csmf_malaria_01to04[which(is.na(dat$csmf_malaria_01to04))] <- 0

# Tidy up
dat <- dat[, c("ISO3", "Year", "csmf_malaria_01to04")]
dat <- dat[order(dat$ISO3, dat$Year),]
rownames(dat) <- NULL

###################################################################
######################### BEGIN-OUTPUTS ###########################
###################################################################

frac_malaria_01to04 <- dat

# Save output(s)
write.csv(frac_malaria_01to04, paste("./gen/prediction/input/frac_malaria_01to04.csv", sep=""), row.names = FALSE)

###################################################################
######################### END-OUTPUTS #############################
###################################################################
