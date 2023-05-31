###################################################################
# Create session variables
###################################################################

# Session variables are simple strings that are called in multiple other scripts.
# Instead of defining them in each script, we define them once here and save as an RData file.

# Indicator that session variables have been created
sessionVars <- TRUE

# Age group labels
ageLow <- as.numeric(sub("to.*", "", ageGroup))
ageUp <- ageLow + 4
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

# Date of results for visualization
# Can either set manually or use the results generated on the current run (from today).
resDate <- "20230531"
# resDate <- format(Sys.Date(), format="%Y%m%d")

save.image(file = "./gen/data-prep/input/session-variables.RData")

