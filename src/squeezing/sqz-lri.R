
###################################################################
########################## BEGIN-INPUTS ###########################
###################################################################

# Load packages and session variables if not already loaded
if(!exists("sessionVars")){source("./src/prepare-session.R")
  load("./gen/data-prep/input/session-variables.Rdata")}

# Load input(s)
if(ageGroup == "05to09"){dat <- read.csv("./gen/squeezing/temp/csmf_SqzOthercmpn_05to09.csv")}
if(ageGroup == "10to14"){dat <- read.csv("./gen/squeezing/temp/csmf_SqzOthercmpn_10to14.csv")}

###################################################################
########################## END-INPUTS #############################
###################################################################

# Multiply lri fraction by envelope, subtract TBre deaths to get residual lri deaths
dat$LRIresid <- dat$LRI * dat$Deaths1 - dat$TBre

# Identify country/years where residual lri deaths are lower than min threshold
# Will need to squeeze the single causes for these country/years
v_idSqz <- which(dat$LRIresid < dat$minLRI)

# Divide total lri deaths by sum of (TBre, minLRI)
# The latter quantities need to fit into LRI
# The quotient is how much they must be scaled down to do so
if(length(v_idSqz) > 0){
  v_scalingFactor <- (dat$LRI * dat$Deaths1)[v_idSqz] / (dat$TBre + dat$minLRI)[v_idSqz]
  # Scale deaths
  dat$TBre[v_idSqz] <- dat$TBre[v_idSqz] * v_scalingFactor
  dat$LRIresid[v_idSqz] <- dat$minLRI[v_idSqz] * v_scalingFactor
  
  # Convert to fractions
  # If there are zero crisis-free deaths, recode fraction as zero
  dat$TBre <- dat$TBre/dat$Deaths1
  dat$TBre[is.na(dat$TBre)] <- 0
  dat$LRI <- dat$LRIresid/dat$Deaths1
  dat$LRI[is.na(dat$LRI)] <- 0
  
  # Final TB fraction
  dat$TB <- apply(dat[, c("TB", "TBre")], 1, sum)
} 

# Remove unnecessary objects
rm(v_idSqz, v_scalingFactor)

###################################################################
######################### BEGIN-OUTPUTS ###########################
###################################################################

# Save output(s)
write.csv(dat, paste("./gen/squeezing/temp/csmf_SqzLri_", ageGroup, ".csv", sep=""), row.names = FALSE)

###################################################################
######################### END-OUTPUTS #############################
###################################################################
