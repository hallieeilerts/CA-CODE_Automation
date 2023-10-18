################################################################################
#' @description Rename columns
#' @return Data frame with c("ISO3", "Year", "dth_malaria_05to19")
################################################################################
#' Libraries
require(readstata13)
#' Inputs
source("./src/prepare-session/set-inputs.R")
source("./src/prepare-session/create-session-variables.R")
dat_malaria_u20_MAP <- read.dta13("./data/single-causes/malaria/malaria_adol_2Nov2022.dta")
key_ctryclass_u20 <- read.csv("./gen/data-management/output/key_ctryclass_u20.csv")
################################################################################

dat <- dat_malaria_u20_MAP

# Add code to create malaria_adol_2Nov2022.dta
# As a place holder, I will just convert malaria_adol_2Nov2022.dta into a csv
# and use the updated name for this object (dth_malaria_05to19)

names(dat)[names(dat) == "iso"] <- "ISO3"
names(dat)[names(dat) == "year"] <- "Year"
names(dat)[names(dat) == "malcases"] <- "dth_malaria_05to19"

# Check that all expected countries are included --------------------------

if(sum(!(unique(key_ctryclass_u20$ISO3) %in% dat$ISO3)) > 0){
  warning("Not all countries included in data input.")
  write.table(sort(unique(key_ctryclass_u20$WHOname)[!(unique(key_ctryclass_u20$ISO3) %in% dat$ISO3)]), 
              "./gen/data-management/audit/missing_malaria.txt")
}

# Fill in zeros for missing country-years, if necessary

# Create data frame for countries/years of interest
# For HIV data, HMM and LMM countries and China
df_ctryyears <- data.frame(ISO3 = rep(key_ctryclass_u20$ISO3, each = length(Years)),
                           Year = rep(Years))

# Merge onto malaria data, identifying missing countries/years
dat <- merge(dat, df_ctryyears, by = idVars[1:2], all = TRUE)

# Recode missing malaria as 0
dat$dth_malaria_05to19[which(is.na(dat$dth_malaria_05to19))] <- 0

# Tidy up
dat <- dat[, c("ISO3", "Year", "dth_malaria_05to19")]
rownames(dat) <- NULL


###################################################################
######################### BEGIN-OUTPUTS ###########################
###################################################################

dat_malaria_5to19 <- dat

# Save output(s)
write.csv(dat_malaria_5to19, paste("./gen/prediction/input/dat_malaria_05to19.csv", sep=""), row.names = FALSE)

###################################################################
######################### END-OUTPUTS #############################
###################################################################
