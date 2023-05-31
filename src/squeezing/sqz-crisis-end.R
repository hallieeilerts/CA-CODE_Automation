
###################################################################
########################## BEGIN-INPUTS ###########################
###################################################################

# Load packages and session variables if not already loaded
if(!exists("sessionVars")){source("./src/prepare-session.R")
  load("./gen/data-prep/input/session-variables.Rdata")}

# Load input(s)
if(ageGroup == "05to09"){dat <- read.csv("./gen/squeezing/temp/csmf_SqzLri_05to09.csv")
                         key_cod <- read.csv("./gen/data-prep/output/key_cod_05to09.csv")}
if(ageGroup == "10to14"){dat <- read.csv("./gen/squeezing/temp/csmf_SqzLri_10to14.csv")
                         key_cod <- read.csv("./gen/data-prep/output/key_cod_10to14.csv")}
if(ageGroup == "15to19f"){dat <- read.csv("./gen/squeezing/temp/csmf_SqzOthercmpn_15to19f.csv")
                          key_cod <- read.csv("./gen/data-prep/output/key_cod_15to19.csv")}
if(ageGroup == "15to19m"){dat <- read.csv("./gen/squeezing/temp/csmf_SqzOthercmpn_15to19m.csv")
                          key_cod <- read.csv("./gen/data-prep/output/key_cod_15to19.csv")}

###################################################################
########################## END-INPUTS #############################
###################################################################

cod <- unique(key_cod$Reclass)  # Vector with ALL CAUSES OF DEATH (including single-cause estimates)
cod <- cod[!cod %in% c("Other", "Undetermined")]

# Add crisis-free deaths with endemic CollectVio and NatDis
v_deaths <- dat$Deaths1 + dat$CollectVio + dat$NatDis

# Calculate fraction of endemic collective violence (Pro-rata squeeze)
dat$CollectVio <- dat$CollectVio/v_deaths

# Calculate fraction of endemic natural disaster (Pro-rata squeeze)
dat$NatDis <- dat$NatDis/v_deaths

# Squeeze other causes into remaining fraction
dat[, paste(cod[which(!cod %in% c("CollectVio", "NatDis"))])] <- 
  dat[, paste(cod[which(!cod %in% c("CollectVio", "NatDis"))])] * (1 - dat$CollectVio - dat$NatDis)

# Remove unnecessary objects
rm(key_cod, cod, v_deaths)

###################################################################
######################### BEGIN-OUTPUTS ###########################
###################################################################

# Save output(s)
write.csv(dat, paste("./gen/squeezing/temp/csmf_SqzCrisisend_", ageGroup, ".csv", sep=""), row.names = FALSE)

###################################################################
######################### END-OUTPUTS #############################
###################################################################