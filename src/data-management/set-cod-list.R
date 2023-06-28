################################################################################
## @Purpose - Sets age-specific COD list for age-sex group under analysis.
## @Date - 2023-06-27
## @Author - Hallie Eilerts-Spinelli
################################################################################

# Clear environment
rm(list = ls())

# Install packages and load libraries

# Load inputs
source("./src/set-inputs.R")
source("./src/create-session-variables.R")
if(ageGroup == "05to09"){dat <- read.csv("./data/classification-keys/ReclassifiedCOD5to9.csv")}
if(ageGroup == "10to14"){dat <- read.csv("./data/classification-keys/ReclassifiedCOD10to14.csv")}
if(ageGroup %in% c("15to19f", "15to19m")){dat <- read.csv("./data/classification-keys/ReclassifiedCOD15to19.csv")}

################################################################################


# Save output(s)
write.csv(dat, paste("./gen/data-prep/output/key_cod_",ageGroup, ".csv", sep=""), row.names = FALSE)
