
###################################################################
########################## BEGIN-INPUTS ###########################
###################################################################

# Load packages and session variables if not already loaded
if(!exists("sessionVars")){source("./src/prepare-session.R")
  load("./gen/data-prep/input/session-variables.Rdata")}

# Load input(s)
if(ageGroup == "05to09"){dat <- read.csv("./gen/squeezing/temp/csmf_SqzCrisisend_05to09.csv")
                         datCHN <- read.csv("./gen/squeezing/temp/csmf_SqzOthercmpn_05to09CHN.csv")
                         key_cod <- read.csv("./gen/data-prep/output/key_cod_05to09.csv")}
if(ageGroup == "10to14"){dat <- read.csv("./gen/squeezing/temp/csmf_SqzCrisisend_10to14.csv")
                         datCHN <- read.csv("./gen/squeezing/temp/csmf_SqzOthercmpn_10to14CHN.csv")
                         key_cod <- read.csv("./gen/data-prep/output/key_cod_10to14.csv")}
if(ageGroup == "15to19f"){dat <- read.csv("./gen/squeezing/temp/csmf_SqzCrisisend_15to19f.csv")
                          datCHN <- read.csv("./gen/squeezing/temp/csmf_SqzOthercmpn_15to19fCHN.csv")
                          key_cod <- read.csv("./gen/data-prep/output/key_cod_15to19.csv")}
if(ageGroup == "15to19m"){dat <- read.csv("./gen/squeezing/temp/csmf_SqzCrisisend_15to19m.csv")
                          datCHN <- read.csv("./gen/squeezing/temp/csmf_SqzOthercmpn_15to19mCHN.csv")
                          key_cod <- read.csv("./gen/data-prep/output/key_cod_15to19.csv")}

###################################################################
########################## END-INPUTS #############################
###################################################################

cod <- unique(key_cod$Reclass)  # Vector with ALL CAUSES OF DEATH (including single-cause estimates)
cod <- cod[!cod %in% c("Other", "Undetermined")]

fn_sqz_crisisepi <- function(dat, cod){ #  key_cod$Reclass
  
  cod <- unique(cod)
  cod <- cod[!cod %in% c("Other", "Undetermined")]
  
  # Transform fractions into deaths
  dat[, paste(cod)] <- dat[, paste(cod)] * dat$Deaths1
  
  # Identify country/years where there are epidemic deaths
  v_idEpi <- which(dat$epi_colvio + dat$epi_natdis != 0)
  
  # Calculate proportion of epidemic deaths that are colvio versus natdis
  if(length(v_idEpi) > 0){
    dat[v_idEpi, c("epi_colvio", "epi_natdis")] <- dat[v_idEpi, c("epi_colvio", "epi_natdis")]/(dat$epi_colvio[v_idEpi] + dat$epi_natdis[v_idEpi])  
  }
  
  # Distribute epidemic deaths proportionally by cause
  # Multiply proportion of colvio/natdis deaths by difference between all-cause and crisis-free envelopes
  # Add to endemic deaths for those causes.
  dat$CollectVio <- dat$CollectVio + dat$epi_colvio * (dat$Deaths2 - dat$Deaths1)
  dat$NatDis <- dat$NatDis + dat$epi_natdis * (dat$Deaths2 - dat$Deaths1)
  # dat$OtherCMPN <- dat$OtherCMPN + dat$epi_othercd * (dat$Deaths2 - dat$Deaths1)
  
  # Distribute epidemic deaths attributed to all causes pro-rata
  v_idEpi <- which(dat$epi_colvio + dat$epi_natdis == 0 & dat$Deaths2 > dat$Deaths1)
  if(length(v_idEpi) > 0){
    # Using crisis-included envelope, convert deaths to CSMFs
    dat[v_idEpi, paste(cod)] <- dat[v_idEpi, paste(cod)] / dat$Deaths2[v_idEpi]
    # These CSMFs will not add up to 1 because the all-cause epidemic deaths were not included in the numerator. 
    # Normalize the CSMFs so they add up to 1.
    dat[v_idEpi, paste(cod)] <- dat[v_idEpi, paste(cod)] / rowSums(dat[v_idEpi, paste(cod)], na.rm = T)
    # Convert fractions back to deaths using crisis-included envelope
    dat[v_idEpi, paste(cod)] <- dat[v_idEpi, paste(cod)] * dat$Deaths2[v_idEpi]
  }
  return(dat)
}

dat <- fn_sqz_crisisepi(dat, cod)
datCHN <- fn_sqz_crisisepi(datCHN, cod)

###################################################################
######################### BEGIN-OUTPUTS ###########################
###################################################################

# Save output(s)
write.csv(dat, paste("./gen/squeezing/temp/dth_SqzCrisisepi_", ageGroup, ".csv", sep=""), row.names = FALSE)
write.csv(datCHN, paste("./gen/squeezing/temp/dth_SqzCrisisepi_", ageGroup, "CHN.csv", sep=""), row.names = FALSE)
rm(datCHN)

###################################################################
######################### END-OUTPUTS #############################
###################################################################