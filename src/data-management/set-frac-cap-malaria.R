################################################################################
#' @description Rename columns
#' @return Data frame with c("ISO3", "Year", "dth_malaria_5to19")
################################################################################
#' Libraries
require(readstata13)
#' Inputs
csmf_01to04 <- read.dta13("./data/mortality-fractions/child_cod_2000-2021.dta")
################################################################################

dat <- csmf_01to04

# Select countries (exclude regions)
dat <- subset(dat, !is.na(iso3))
dat <- subset(dat, iso3 != "NA")
# Refer to "child_cod_2000-2021.xls" readme tab to identify column name for postneonatal malaria CSMF
dat$csmf_malaria_01to04 <- dat$post8/dat$pnd
dat$csmf_malaria_01to04[is.na(dat$csmf_malaria_01to04)] <- 0
dat <- dat[,c("iso3", "year", "csmf_malaria_01to04")]

# Rename variables of interest
names(dat) <- c("ISO3", "Year", "csmf_malaria_01to04")
rownames(dat) <- NULL

# !!!!! institute check that all countries are present.

#------------------------------------#
## **Note**
## In "PointEstimates001", the malaria fraction is calculated in the following way:
# dat$pct_malaria_1to59 <- dat$post8 / rowSums(dat[, grep('post', names(dat))], na.rm = T)
# dat <- dat[,c("iso3", "year", "pct_malaria_1to59")]
# View(dat[,c("iso3","year","pnd","post8", 
#             #"fpost2","fpost3","fpost5","fpost6","fpost7",
#             #"fpost8","fpost9","fpost10","fpost11","fpost12",
#             #"fpost13","fpost15","fpost16","fpost17","fpost18",
#             "rpost2","rpost3","rpost5","rpost6", "rpost7", "rpost8","rpost9","rpost10","rpost11","rpost12","rpost13",
#             "rpost15","rpost16","rpost17", "rpost18" )])
## This is a mistake, because it grep's all the "post" columns which includes fractions, rates, and deaths.
## (Though this is not evident from just looking at the file quickly in STATA, because it only previews the deaths columns)
## This could account for some differences with Pancho's results.
## These differences should be relatively small since the rates per 1,000 and fractions shouldn't contribute too much to the denominator compared to death counts.

## However, I've also found that it's not safe to use the "fpost" columns in this "child_cod_2000-2021.dta". See below.
## All death count columns for postneonates
#v_dths <- c("post2", "post3", "post5", "post6", "post7", "post8", "post9", "post10", "post11", "post12", "post13", "post15", "post16", "post17", "post18")
## All fraction columns for postneonates
#v_frac <- c("fpost2","fpost3","fpost5","fpost6","fpost7","fpost8","fpost9","fpost10","fpost11","fpost12","fpost13","fpost15","fpost16","fpost17","fpost18")
#test <- subset(dat, iso3 == "BFA")
## Calculating fractions manually
#head(test$pnd)
#head(rowSums(test[,v_dths]))
#head(test$post8/test$pnd)
#head(test$post8/rowSums(test[v_dths]))
## Does not match fpost8
#head(test$fpost8)
## and fpost columns do not add up to 1
#rowSums(test[,v_frac])
## my manually calculated fractions do...
#rowSums(head(test[,v_dths])/head(rowSums(test[,v_dths])))
## So in conclusion, looks like fpost are wrong in this file. 
## The fractions are correctly calculated in another (more final?) results file, "Uncertainty_country_post_20230810.xlsx" 
## (and the same as my manually calculated ones)
## So I will use my manually calculated fractions.
#------------------------------------#

###################################################################
######################### BEGIN-OUTPUTS ###########################
###################################################################

frac_malaria_01to04 <- dat

# Save output(s)
write.csv(frac_malaria_01to04, paste("./gen/prediction/input/frac_malaria_01to04.csv", sep=""), row.names = FALSE)

###################################################################
######################### END-OUTPUTS #############################
###################################################################
