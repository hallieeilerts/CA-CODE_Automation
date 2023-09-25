################################################################################
#' @description Sets age-specific COD list for age-sex group under analysis.
#' @return Data frame with age-specific CODs with different levels of classification.
################################################################################
#' Libraries
#' Inputs
source("./src/prepare-session/set-inputs.R")
source("./src/prepare-session/create-session-variables.R")
if(ageGroup == "05to09"){key_cod <- read.csv("./data/classification-keys/ReclassifiedCOD5to9.csv")}
if(ageGroup == "10to14"){key_cod <- read.csv("./data/classification-keys/ReclassifiedCOD10to14.csv")}
if(ageGroup %in% c("15to19f", "15to19m")){key_cod <- read.csv("./data/classification-keys/ReclassifiedCOD15to19.csv")}
################################################################################

# Save output(s)
write.csv(key_cod, paste("./gen/data-management/output/key_cod_",ageGroup, ".csv", sep=""), row.names = FALSE)
