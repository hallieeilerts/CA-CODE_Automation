
###################################################################
######################## BEGIN-FUNCTIONS ##########################
###################################################################

fn_prepare_sqz <- function(dat, env, dth_tb, dth_hiv, dth_crisis, dth_meas, minCD, minLRI){
  
  # Load input(s)
  # datHMM <- read.csv(paste("./gen/prediction/output/csmf_CapMalaria_",ageGroup, "HMM.csv", sep=""))
  # datLMM <- read.csv(paste("./gen/prediction/output/csmf_SetMalaria_",ageGroup, "LMM.csv", sep=""))
  # datIGME <- read.csv(paste("./gen/data-prep/output/env_", ageGroup, ".csv", sep=""))
  # dth_tb <- read.csv(paste("./gen/squeezing/input/dth_tb_", ageGroup, ".csv", sep=""))
  # dth_hiv <- read.csv(paste("./gen/squeezing/input/dth_hiv_", ageGroup, ".csv", sep=""))
  # dth_crisis <- read.csv(paste("./gen/squeezing/input/dth_crisis_", ageGroup, ".csv", sep=""))
  # if(ageGroup == "05to09"){dth_meas <- read.csv(paste("./gen/squeezing/input/dth_meas_05to09.csv", sep=""))}
  # minCD <- readRDS(paste("./gen/squeezing/input/minfrac_cd_", ageGroup, ".rds", sep=""))
  # minLRI <- readRDS(paste("./gen/squeezing/input/minfrac_lri_", ageGroup, ".rds", sep=""))
  
  # Merge on IGME envelopes
  dat <- merge(dat, env, by = idVars, all.x = T)
  
  # Merge on TB
  dat <- merge(dat, dth_tb, by = idVars, all.x = T)

  # Merge on HIV
  dat <- merge(dat, dth_hiv, by = idVars, all.x = T)
  
  # Merge on measles
  if(!is.null(dth_meas)){
    dat <- merge(dat, dth_meas, by = idVars, all.x = T)
  }

  # Merge on crisis
  dat <- merge(dat, dth_crisis, by = idVars, all.x = T)
  
  # Merge on minimum CD fraction and convert to deaths
  dat$minCD <- dat$Deaths1 * minCD
  
  # Merge on minimum LRI fraction and convert to deaths
  if(!is.null(minLRI)){dat$minLRI <- dat$Deaths1 * minLRI}
  
  #------------------------#
  # 2023.02.23 PATCH
  # Adjust epi deaths to envelopes: All countries
  v_idEpi <- which(dat$epi_colvio + dat$epi_natdis > 0 & dat$Deaths2 == dat$Deaths1)
  if (length(v_idEpi) > 0) {
    dat$epi_colvio[v_idEpi] <- 0
    dat$epi_natdis[v_idEpi] <- 0
  }
  #------------------------#
  
  # Exclude country-years with no deaths
  datNODEATHS <- dat[which(dat$Deaths1 == 0), ]
  dat <- dat[which(dat$Deaths1 > 0), ]
  rownames(dat) <- NULL
  
  return(dat)
  # Save output(s)
  #write.csv(dat, paste("./gen/squeezing/temp/csmf_AddSinglecause_", ageGroup, ".csv", sep=""), row.names = FALSE)
}

fn_prepare_sqz_china <- function(dat, env, dth_hiv, dth_crisis, minCD){
  
  # Merge on IGME envelopes
  dat <- merge(dat, env, by = idVars, all.x = T)
  
  # Merge on HIV
  dat <- merge(dat, dth_hiv, by = idVars, all.x = T)
  
  # Merge on crisis
  dat <- merge(dat, dth_crisis[, names(dth_crisis)[!names(dth_crisis) %in% c("CollectVio", "NatDis")]], 
               by = idVars, all.x = T)
  
  # Merge on minimum CD fraction and convert to deaths
  dat$minCD <- dat$Deaths1 * minCD
  
  #------------------------#
  # 2023.02.23 PATCH
  # Adjust epi deaths to envelopes: China
  idEpi <- which(dat$epi_colvio + dat$epi_natdis > 0 &
                   dat$Deaths2 == dat$Deaths1)
  if (length(idEpi) > 0) {
    dat$epi_colvio[idEpi] <- 0
    dat$epi_natdis[idEpi] <- 0
  }
  #------------------------#
  
  return(dat)
  # Save output(s)
  #write.csv(dat, paste("./gen/squeezing/temp/csmf_AddSinglecause_", ageGroup, "CHN.csv", sep=""), row.names = FALSE)
}

fn_sqz_othercmpn <- function(dat){
  
  # Load input(s)
  #dat <- read.csv(paste("./gen/squeezing/temp/csmf_AddSinglecause_",ageGroup, ".csv", sep=""))
  
  # Multiply othercmpn fraction by envelope, subtract (TB, HIV, Measles) deaths to get residual othercmpn deaths
  if("Measles" %in% names(dat)){
    dat$OCDresid <- (dat$OtherCMPN * dat$Deaths1) - dat$TB - dat$HIV - dat$Measles
  }else{
    dat$OCDresid <- (dat$OtherCMPN * dat$Deaths1) - dat$TB - dat$HIV
  }
  
  # Identify country/years where residual othercmpn deaths are lower than min threshold
  # Will need to squeeze the single causes for these country/years
  v_idSqz <- which(dat$OCDresid < dat$minCD)
  
  # Divide total othercmpn deaths by sum of (TB, HIV, Measles, minCD)
  # The latter quantities need to fit into othercmpn
  # The quotient is how much they must be scaled down to do so
  if (length(v_idSqz) > 0) {
    if("Measles" %in% names(dat)){
      v_scalingFactor <- (dat$OtherCMPN * dat$Deaths1)[v_idSqz] / (dat$TB + dat$HIV + dat$Measles + dat$minCD)[v_idSqz]
      dat$Measles[v_idSqz] <- dat$Measles[v_idSqz] * v_scalingFactor
    }else{
      v_scalingFactor <- (dat$OtherCMPN * dat$Deaths1)[v_idSqz] / (dat$TB + dat$HIV + dat$minCD)[v_idSqz]
    }
    # Scale deaths
    dat$TB[v_idSqz] <- dat$TB[v_idSqz] * v_scalingFactor
    dat$HIV[v_idSqz] <- dat$HIV[v_idSqz] * v_scalingFactor
    dat$OCDresid[v_idSqz] <- dat$minCD[v_idSqz] * v_scalingFactor
    # range(dat$TB + dat$HIV + dat$Measles + dat$OCDresid - dat$OtherCMPN * dat$Deaths1)
    # range(dat$TB[v_idSqz] + dat$HIV[v_idSqz] + dat$Measles[v_idSqz] + dat$OCDsq[v_idSqz] - (dat$OtherCMPN * dat$Deaths1)[v_idSqz])
  }
  
  # Convert to fractions
  # If there are zero crisis-free deaths, recode fraction as zero
  if("Measles" %in% names(dat)){
    dat$Measles <- dat$Measles / dat$Deaths1
    dat$Measles[is.na(dat$Measles)] <- 0
  }
  dat$TB <- dat$TB/dat$Deaths1
  dat$TB[is.na(dat$TB)] <- 0
  dat$HIV <- dat$HIV/dat$Deaths1
  dat$HIV[is.na(dat$HIV)] <- 0
  dat$OtherCMPN <- dat$OCDresid/dat$Deaths1
  dat$OtherCMPN[is.na(dat$OtherCMPN)] <- 0
  
  return(dat)
  # Save output(s)
  #write.csv(dat, paste("./gen/squeezing/temp/csmf_SqzOthercmpn_", ageGroup, ".csv", sep=""), row.names = FALSE)

}

fn_sqz_othercmpn_china <- function(dat){
  
  # Load input(s)
  #dat <- read.csv(paste("./gen/squeezing/temp/csmf_AddSinglecause_",ageGroup, "CHN.csv", sep=""))
  
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
  
  return(dat)
  # Save output(s)
  #write.csv(dat, paste("./gen/squeezing/temp/csmf_SqzOthercmpn_", ageGroup, "CHN.csv", sep=""), row.names = FALSE)
  
}

fn_sqz_lri <- function(dat){
  
  # Load input(s)
  #dat <- read.csv(paste("./gen/squeezing/temp/csmf_SqzOthercmpn_",ageGroup, ".csv", sep=""))
  
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
  
  return(dat)
  # Save output(s)
  #write.csv(dat, paste("./gen/squeezing/temp/csmf_SqzLri_", ageGroup, ".csv", sep=""), row.names = FALSE)
}

fn_sqz_crisisend <- function(dat){
  
  # Load input(s)
  #dat <- read.csv(paste("./gen/squeezing/temp/csmf_SqzLri_", ageGroup, ".csv", sep=""))
  #key_cod <- read.csv(paste("./gen/data-prep/output/key_cod_", ageGroup, ".csv", sep=""))
  
  # Vector with all causes of death (including single-cause estimates)
  cod <- unique(key_cod$Reclass)  
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
  
  return(dat)
  # Save output(s)
  #write.csv(dat, paste("./gen/squeezing/temp/csmf_SqzCrisisend_", ageGroup, ".csv", sep=""), row.names = FALSE)
}

fn_sqz_crisisepi <- function(dat){
  
  # Load input(s)
  #dat <- read.csv(paste("./gen/squeezing/temp/csmf_SqzCrisisend_", ageGroup, ".csv", sep=""))
  #datCHN <- read.csv(paste("./gen/squeezing/temp/csmf_SqzOthercmpn_", ageGroup, "CHN.csv", sep=""))
  #key_cod <- read.csv(paste("./gen/data-prep/output/key_cod_", ageGroup, ".csv", sep=""))
  
  # Vector with all causes of death (including single-cause estimates)
  cod <- unique(key_cod$Reclass)  
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

fn_add_measepi <- function(dat){
  
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
  
  return(dat)
}

fn_format_sqz_output <- function(dat, datCHN, csmf){
  
  cod <- unique(key_cod$Reclass)  # Vector with ALL CAUSES OF DEATH (including single-cause estimates)
  cod <- cod[!cod %in% c("Other", "Undetermined")]
  
  # Combine China with HMM/LMM countries
  dat <- bind_rows(dat, datCHN)
  
  # Back-transform deaths into fractions
  dat[, paste(cod)] <- dat[, paste(cod)]/dat$Deaths2
  
  # Checks
  if(any(is.na(rowSums(dat[, paste(cod)])))){
    stop("CSMFs contain NA")
  }
  if(any(round(rowSums(dat[, paste(cod)]),5) != 1)){
    warning("CSMFs do not add up to 1")
  }
  # print(round(rowSums(dat[, paste(cod)]),7), digits = 20)
  #table(rowSums(dat[, paste(cod)]))
  #table(round(rowSums(dat[, paste(cod)]),5))
  #dat[which(round(rowSums(dat[, paste(cod)]),5) == 0.99942),]
  #dat[which(rowSums(dat[, paste(cod)]) != 1),]
  #foo <- dat[which(rowSums(dat[, paste(cod)]) > 1.18),]
  #foo[, c("ISO3",paste(cod))]
  
  # Select columns of interest
  dat <- dat[, c("ISO3", "Year", "Sex", "Deaths2", "Rate2", paste(cod))]
  
  # Incorporate country-year with 0 crisis-free deaths
  # These were excluded in fn_prepare_sqz()
  datNODEATHS <- csmf[which(csmf$Deaths1 == 0), ]
  if (nrow(datNODEATHS) > 0) {
    datNODEATHS <- datNODEATHS[, names(dat)]
    datNODEATHS[, paste(cod)] <- 0
    dat <- rbind(dat, datNODEATHS)  
  }
  
  # Tidy up
  names(dat)[names(dat) == "Deaths2"] <- "Deaths"
  names(dat)[names(dat) == "Rate2"] <- "Rate"
  dat <- dat[order(dat$ISO3, dat$Year, dat$Sex), ]
  rownames(dat) <- NULL
  
  return(dat)
}


###################################################################
########################## END-FUNCTIONS ##########################
###################################################################

