################################################################################
#' @description Create variables related to the age-sex group under analysis.
#' @return Strings, booleans defined below
################################################################################
#' Libraries
#' Inputs
source("./src/prepare-session/set-inputs.R")
################################################################################

if(!exists("ageGroup")){
  stop("ageGroup does not exist. Choose ageGroup in set-inputs.R")
}
if(!exists("Years")){
  stop("Years does not exist. Choose Years in set-inputs.R")
}

# Indicator that session variables have been created
sessionVars <- TRUE

# Age group labels
ageLow <- as.numeric(sub("to.*", "", ageGroup))
ageUp <- as.numeric(gsub("[[:alpha:]]", "", sub(".*to", "", ageGroup)))
ageUp  <- ifelse(ageGroup == "00to28", 1/12, ageUp)
ageLow <- ifelse(ageGroup == "01to04", 28/365.25, ageLow)
ageUp  <- ifelse(ageGroup == "01to04", 4, ageUp)

# Sex split
sexSplit <- ifelse(ageLow == 15, TRUE, FALSE)

# Create variable for sex
if(sexSplit){
  if (ageGroup == "15to19f") sexLabel <- "Female"
  if (ageGroup == "15to19m") sexLabel <- "Male"
}else{
  sexLabel <- "Both"
}
sexLabels <- c("Both", "Female", "Male") 

# Create suffix for sex
if(sexSplit){
  if (ageGroup == "15to19f") sexSuffix <- "f"
  if (ageGroup == "15to19m") sexSuffix <- "m"
}else{
  sexSuffix <- "mf"
}

# Create variable for respiratory TB
respTB <- ifelse(ageGroup %in% c("05to09", "10to14"), TRUE, FALSE)

# Variables to uniquely identify records
idVars <- c("ISO3", "Year", "Sex")

# Vector with COD in correct order
codAll <- c("Measles", "Maternal", "HIV", "LRI",  "TB", "Diarrhoeal", "Malaria", "OtherCMPN",
            "Congenital", "Cardiovascular", "Digestive", "Neoplasms", "OtherNCD",
            "InterpVio","SelfHarm", "Drowning", "RTI", "OtherInj", "NatDis", "CollectVio")    

# Save names of all session variables
sessionVarsList <- ls()
