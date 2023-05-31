###################################################################
########################## BEGIN-INPUTS ###########################
###################################################################

# Load packages and session variables if not already loaded
if(!exists("sessionVars")){source("./src/prepare-session.R")
  load("./gen/data-prep/input/session-variables.Rdata")}

# Load input(s)
if(ageGroup == "05to09"){dat <- read.csv("./data/classification-keys/ReclassifiedCOD5to9.csv")}
if(ageGroup == "10to14"){dat <- read.csv("./data/classification-keys/ReclassifiedCOD10to14.csv")}
if(ageGroup %in% c("15to19f", "15to19m")){dat <- read.csv("./data/classification-keys/ReclassifiedCOD15to19.csv")}

###################################################################
########################## END-INPUTS #############################
###################################################################

# Do I need to save this vector as an output?
# Vector with all causes of death in the correct order ## THESE ARE ONLY FOR 5-19
#codAll <- c("Measles", "Maternal", "HIV", "LRI",  "TB", "Diarrhoeal", "Malaria", "OtherCMPN",
#            "Congenital", "Cardiovascular", "Digestive", "Neoplasms", "OtherNCD",
#            "InterpVio","SelfHarm", "Drowning", "RTI", "OtherInj", "NatDis", "CollectVio")

key_cod <- dat
ageGroup2 <- ifelse(ageGroup %in% c("15to19f", "15to19m"), "15to19", ageGroup)

###################################################################
######################### BEGIN-OUTPUTS ###########################
###################################################################

# Save output(s)
write.csv(key_cod, paste("./gen/data-prep/output/key_cod_",ageGroup2, ".csv", sep=""), row.names = FALSE)
rm(ageGroup2)

###################################################################
######################### END-OUTPUTS #############################
###################################################################
