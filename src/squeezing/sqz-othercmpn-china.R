
###################################################################
########################## BEGIN-INPUTS ###########################
###################################################################

# Load packages and session variables if not already loaded
if(!exists("sessionVars")){source("./src/prepare-session.R")
  load("./gen/data-prep/input/session-variables.Rdata")}

# Load input(s)
if(ageGroup == "05to09"){dat <- read.csv("./gen/squeezing/temp/csmf_AddSinglecause_05to09CHN.csv")}
if(ageGroup == "10to14"){dat <- read.csv("./gen/squeezing/temp/csmf_AddSinglecause_10to14CHN.csv")}
if(ageGroup == "15to19f"){dat <- read.csv("./gen/squeezing/temp/csmf_AddSinglecause_15to19fCHN.csv")}
if(ageGroup == "15to19m"){dat <- read.csv("./gen/squeezing/temp/csmf_AddSinglecause_15to19mCHN.csv")}

###################################################################
########################## END-INPUTS #############################
###################################################################

# Multiply othercmpn fraction by envelope, subtract HIV deaths to get residual othercmpn deaths
dat$OCDresid <- (dat$OtherCMPN * dat$Deaths1) - dat$HIV

# Identify country/years where residual othercmpn deaths are lower than min threshold
# Will need to squeeze the single causes for these country/years
v_idSqz <- which(dat$OCDresid < dat$minCD)

# Divide total othercmpn deaths by sum of (HIV, minCD)
# The latter quantities need to fit into othercmpn
# The quotient is how much they must be scaled down to do so
if (length(v_idSqz) > 0) {
  v_scalingFactor <- (dat$OtherCMPN * dat$Deaths1)[v_idSqz] / (dat$HIV + dat$minCD)[v_idSqz]
  # Scale deaths
  dat$HIV[v_idSqz] <- dat$HIV[v_idSqz] * v_scalingFactor
  dat$OCDresid[v_idSqz] <- dat$minCD[v_idSqz] * v_scalingFactor
}

# Convert to fractions
# If there are zero crisis-free deaths, recode fraction as zero
dat$HIV <- dat$HIV/dat$Deaths1
dat$HIV[is.na(dat$HIV)] <- 0
dat$OtherCMPN <- dat$OCDresid/dat$Deaths1
dat$OtherCMPN[is.na(dat$OtherCMPN)] <- 0

# Remove unnecessary objects
rm(v_idSqz, v_scalingFactor)

###################################################################
######################### BEGIN-OUTPUTS ###########################
###################################################################

# Save output(s)
write.csv(dat, paste("./gen/squeezing/temp/csmf_SqzOthercmpn_", ageGroup, "CHN.csv", sep=""), row.names = FALSE)

###################################################################
######################### END-OUTPUTS #############################
###################################################################
