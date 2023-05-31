
###################################################################
########################## BEGIN-INPUTS ###########################
###################################################################

# Load packages and session variables if not already loaded
if(!exists("sessionVars")){source("./src/prepare-session.R")
  load("./gen/data-prep/input/session-variables.Rdata")}

# Load input(s)
if(ageGroup == "05to09"){dat <- read.csv("./gen/squeezing/temp/dth_SqzCrisisepi_05to09.csv")
                         key_cod <- read.csv("./gen/data-prep/output/key_cod_05to09.csv")}

###################################################################
########################## END-INPUTS #############################
###################################################################

# Adjust epidemic measles so that total measles is not smaller than 0
v_idMeasles <- which(dat$epi_meas < 0 & (dat$Measles + dat$epi_meas) < 0) 
if(length(v_idMeasles) > 0){
  # Recode epidemic measles as the negative of endemic measles
  dat$epi_meas[v_idMeasles] <- -dat$Measles[v_idMeasles]
}

# For all country-years with epidemic measles
v_idMeasles <- which(dat$epi_meas != 0)
if (length(v_idMeasles) > 0) {
  
  # Recover denominator from crisis-included deaths and rates
  v_px <- dat$Deaths2[v_idMeasles]/dat$Rate2[v_idMeasles]
  
  # Combine endemic and epidemic measles deaths
  # Due to the adjustment above, the lowest value of total measles will be 0
  dat$Measles[v_idMeasles] <- (dat$Measles + dat$epi_meas)[v_idMeasles]
  
  # Add epidemic measles to the top of the crisis-included envelope
  dat$Deaths2[v_idMeasles] <- (dat$Deaths2 + dat$epi_meas)[v_idMeasles]  
  
  # Recalculate the crisis-included mortality rates
  dat$Rate2[v_idMeasles] <- dat$Deaths2[v_idMeasles] / v_px
  
}

# Remove unnecessary objects
rm(v_idMeasles)

###################################################################
######################### BEGIN-OUTPUTS ###########################
###################################################################

# Save output(s)
write.csv(dat, paste("./gen/squeezing/temp/dth_AddMeasepi_", ageGroup, ".csv", sep=""), row.names = FALSE)

###################################################################
######################### END-OUTPUTS #############################
###################################################################