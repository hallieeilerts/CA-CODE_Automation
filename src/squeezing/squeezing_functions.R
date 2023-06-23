
##################################################
####
####   Prepare predicted CSMFs data frame for squeezing
####
##################################################

fn_prepare_sqz <- function(DAT, ENV, DTH_TB, DTH_HIV, DTH_CRISIS, DTH_MEAS, MINCD, MINLRI){
  
  ## DAT         Predicted CSMFs
  ## ENV         IGME envelope for crisis-included and crisis-free deaths and rates
  ## DTH_TB      Single cause data: tuberculosis
  ## DTH_HIV     Single cause data: HIV
  ## DTH_CRISIS  Single cause data: crisis
  ## DTH_MEAS    Single cause data: measles
  ## MINCD       Minimum fraction of communicable disease
  ## MINLRI      Minimum fraction of LRI
  
  # Merge on IGME envelopes
  dat <- merge(DAT, ENV, by = idVars, all.x = T)
  
  # Merge on TB
  dat <- merge(dat, DTH_TB, by = idVars, all.x = T)

  # Merge on HIV
  dat <- merge(dat, DTH_HIV, by = idVars, all.x = T)
  
  # Merge on measles
  if(!is.null(dth_meas)){
    dat <- merge(dat, DTH_MEAS, by = idVars, all.x = T)
  }

  # Merge on crisis
  dat <- merge(dat, DTH_CRISIS, by = idVars, all.x = T)
  
  # Merge on minimum CD fraction and convert to deaths
  dat$minCD <- dat$Deaths1 * MINCD
  
  # Merge on minimum LRI fraction and convert to deaths
  if(!is.null(MINLRI)){dat$minLRI <- dat$Deaths1 * MINLRI}
  
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
  dat <- dat[which(dat$Deaths1 > 0), ]
  # These will be added back in fn_format_sqz_output()
  
  # Tidy up
  rownames(dat) <- NULL
  
  return(dat)
}


##################################################
####
####   Prepare calculated CSMFs data frame for China for squeezing
####
##################################################


fn_prepare_sqz_china <- function(DAT, ENV, DTH_HIV, DTH_CRISIS, MINCD){
  
  ## DAT         Calculated CSMFs
  ## ENV         IGME envelope for crisis-included and crisis-free deaths and rates
  ## DTH_HIV     Single cause data: HIV
  ## DTH_CRISIS  Single cause data: crisis
  ## MINCD       Minimum fraction of communicable disease
  
  # Merge on IGME envelopes
  dat <- merge(DAT, ENV, by = idVars, all.x = T)
  
  # Merge on HIV
  dat <- merge(dat, DTH_HIV, by = idVars, all.x = T)
  
  # Merge on crisis
  dat <- merge(dat, DTH_CRISIS[, names(DTH_CRISIS)[!names(DTH_CRISIS) %in% c("CollectVio", "NatDis")]], 
               by = idVars, all.x = T)
  
  # Merge on minimum CD fraction and convert to deaths
  dat$minCD <- dat$Deaths1 * MINCD
  
  #------------------------#
  # 2023.02.23 PATCH
  # Adjust epi deaths to envelopes: China
  v_idEpi <- which(dat$epi_colvio + dat$epi_natdis > 0 & dat$Deaths2 == dat$Deaths1)
  if (length(v_idEpi) > 0) {
    dat$epi_colvio[v_idEpi] <- 0
    dat$epi_natdis[v_idEpi] <- 0
  }
  #------------------------#
  
  return(dat)
 
}

##################################################
####
####   Squeeze otherCMPN
####
##################################################

fn_sqz_othercmpn <- function(DAT){
  
  ## DAT         CSMFs data frame that has been prepared for squeezing
  
  # Multiply othercmpn fraction by envelope, subtract (TB, HIV, Measles) deaths to get residual othercmpn deaths
  if("Measles" %in% names(DAT)){
    DAT$OCDresid <- (DAT$OtherCMPN * DAT$Deaths1) - DAT$TB - DAT$HIV - DAT$Measles
  }else{
    DAT$OCDresid <- (DAT$OtherCMPN * DAT$Deaths1) - DAT$TB - DAT$HIV
  }
  
  # Identify country/years where residual othercmpn deaths are lower than min threshold
  # Will need to squeeze the single causes for these country/years
  v_idSqz <- which(DAT$OCDresid < DAT$minCD)
  
  # Divide total othercmpn deaths by sum of (TB, HIV, Measles, minCD)
  # The latter quantities need to fit into othercmpn
  # The quotient is how much they must be scaled down to do so
  if (length(v_idSqz) > 0) {
    if("Measles" %in% names(DAT)){
      v_scalingFactor <- (DAT$OtherCMPN * DAT$Deaths1)[v_idSqz] / (DAT$TB + DAT$HIV + DAT$Measles + DAT$minCD)[v_idSqz]
      DAT$Measles[v_idSqz] <- DAT$Measles[v_idSqz] * v_scalingFactor
    }else{
      v_scalingFactor <- (DAT$OtherCMPN * DAT$Deaths1)[v_idSqz] / (DAT$TB + DAT$HIV + DAT$minCD)[v_idSqz]
    }
    # Scale deaths
    DAT$TB[v_idSqz] <- DAT$TB[v_idSqz] * v_scalingFactor
    DAT$HIV[v_idSqz] <- DAT$HIV[v_idSqz] * v_scalingFactor
    DAT$OCDresid[v_idSqz] <- DAT$minCD[v_idSqz] * v_scalingFactor
    # range(DAT$TB + DAT$HIV + DAT$Measles + DAT$OCDresid - DAT$OtherCMPN * DAT$Deaths1)
    # range(DAT$TB[v_idSqz] + DAT$HIV[v_idSqz] + DAT$Measles[v_idSqz] + DAT$OCDsq[v_idSqz] - (DAT$OtherCMPN * DAT$Deaths1)[v_idSqz])
  }
  
  # Convert to fractions
  # If there are zero crisis-free deaths, recode fraction as zero
  if("Measles" %in% names(DAT)){
    DAT$Measles <- DAT$Measles / DAT$Deaths1
    DAT$Measles[is.na(DAT$Measles)] <- 0
  }
  DAT$TB <- DAT$TB/DAT$Deaths1
  DAT$TB[is.na(DAT$TB)] <- 0
  DAT$HIV <- DAT$HIV/DAT$Deaths1
  DAT$HIV[is.na(DAT$HIV)] <- 0
  DAT$OtherCMPN <- DAT$OCDresid/DAT$Deaths1
  DAT$OtherCMPN[is.na(DAT$OtherCMPN)] <- 0
  
  return(DAT)

}

##################################################
####
####   Squeeze otherCMPN for China
####
##################################################

fn_sqz_othercmpn_china <- function(DAT){
  
  ## DAT         CSMFs data frame that has been prepared for squeezing

  # Multiply othercmpn fraction by envelope, subtract HIV deaths to get residual othercmpn deaths
  DAT$OCDresid <- (DAT$OtherCMPN * DAT$Deaths1) - DAT$HIV
  
  # Identify country/years where residual othercmpn deaths are lower than min threshold
  # Will need to squeeze the single causes for these country/years
  v_idSqz <- which(DAT$OCDresid < DAT$minCD)
  
  # Divide total othercmpn deaths by sum of (HIV, minCD)
  # The latter quantities need to fit into othercmpn
  # The quotient is how much they must be scaled down to do so
  if (length(v_idSqz) > 0) {
    v_scalingFactor <- (DAT$OtherCMPN * DAT$Deaths1)[v_idSqz] / (DAT$HIV + DAT$minCD)[v_idSqz]
    # Scale deaths
    DAT$HIV[v_idSqz] <- DAT$HIV[v_idSqz] * v_scalingFactor
    DAT$OCDresid[v_idSqz] <- DAT$minCD[v_idSqz] * v_scalingFactor
  }
  
  # Convert to fractions
  # If there are zero crisis-free deaths, recode fraction as zero
  DAT$HIV <- DAT$HIV/DAT$Deaths1
  DAT$HIV[is.na(DAT$HIV)] <- 0
  DAT$OtherCMPN <- DAT$OCDresid/DAT$Deaths1
  DAT$OtherCMPN[is.na(DAT$OtherCMPN)] <- 0
  
  return(DAT)

}

##################################################
####
####   Squeeze LRI
####
##################################################


fn_sqz_lri <- function(DAT){
  
  ## DAT         CSMFs data frame that has been prepared for squeezing

  # Multiply lri fraction by envelope, subtract TBre deaths to get residual lri deaths
  DAT$LRIresid <- DAT$LRI * DAT$Deaths1 - DAT$TBre
  
  # Identify country/years where residual lri deaths are lower than min threshold
  # Will need to squeeze the single causes for these country/years
  v_idSqz <- which(DAT$LRIresid < DAT$minLRI)
  
  # Divide total lri deaths by sum of (TBre, minLRI)
  # The latter quantities need to fit into LRI
  # The quotient is how much they must be scaled down to do so
  if(length(v_idSqz) > 0){
    v_scalingFactor <- (DAT$LRI * DAT$Deaths1)[v_idSqz] / (DAT$TBre + DAT$minLRI)[v_idSqz]
    # Scale deaths
    DAT$TBre[v_idSqz] <- DAT$TBre[v_idSqz] * v_scalingFactor
    DAT$LRIresid[v_idSqz] <- DAT$minLRI[v_idSqz] * v_scalingFactor
    
    # Convert to fractions
    # If there are zero crisis-free deaths, recode fraction as zero
    DAT$TBre <- DAT$TBre/DAT$Deaths1
    DAT$TBre[is.na(DAT$TBre)] <- 0
    DAT$LRI <- DAT$LRIresid/DAT$Deaths1
    DAT$LRI[is.na(DAT$LRI)] <- 0
    
    # Final TB fraction
    DAT$TB <- apply(DAT[, c("TB", "TBre")], 1, sum)
  } 
  
  return(DAT)

}

##################################################
####
####   Squeeze endemic crisis
####
##################################################


fn_sqz_crisisend <- function(DAT){
  
  ## DAT         CSMFs data frame that has been prepared for squeezing
  
  # Vector with all causes of death (including single-cause estimates)
  cod <- unique(key_cod$Reclass)  
  cod <- cod[!cod %in% c("Other", "Undetermined")]
  
  # Add crisis-free deaths with endemic CollectVio and NatDis
  v_deaths <- DAT$Deaths1 + DAT$CollectVio + DAT$NatDis
  
  # Calculate fraction of endemic collective violence (Pro-rata squeeze)
  DAT$CollectVio <- DAT$CollectVio/v_deaths
  
  # Calculate fraction of endemic natural disaster (Pro-rata squeeze)
  DAT$NatDis <- DAT$NatDis/v_deaths
  
  # Squeeze other causes into remaining fraction
  DAT[, paste(cod[which(!cod %in% c("CollectVio", "NatDis"))])] <- 
    DAT[, paste(cod[which(!cod %in% c("CollectVio", "NatDis"))])] * (1 - DAT$CollectVio - DAT$NatDis)
  
  return(DAT)

}

##################################################
####
####   Squeeze epidemic crisis
####
##################################################

fn_sqz_crisisepi <- function(DAT){
  
  ## DAT         CSMFs data frame that has been prepared for squeezing
  
  # Vector with all causes of death (including single-cause estimates)
  v_cod <- unique(key_cod$Reclass)  
  v_cod <- v_cod[!v_cod %in% c("Other", "Undetermined")]
  
  # Transform fractions into deaths
  DAT[, paste(v_cod)] <- DAT[, paste(v_cod)] * DAT$Deaths1
  
  # Identify country/years where there are epidemic deaths
  v_idEpi <- which(DAT$epi_colvio + DAT$epi_natdis != 0)
  
  # Calculate proportion of epidemic deaths that are colvio versus natdis
  if(length(v_idEpi) > 0){
    DAT[v_idEpi, c("epi_colvio", "epi_natdis")] <- DAT[v_idEpi, c("epi_colvio", "epi_natdis")]/(DAT$epi_colvio[v_idEpi] + DAT$epi_natdis[v_idEpi])  
  }
  
  # Distribute epidemic deaths proportionally by cause
  # Multiply proportion of colvio/natdis deaths by difference between all-cause and crisis-free envelopes
  # Add to endemic deaths for those causes.
  DAT$CollectVio <- DAT$CollectVio + DAT$epi_colvio * (DAT$Deaths2 - DAT$Deaths1)
  DAT$NatDis <- DAT$NatDis + DAT$epi_natdis * (DAT$Deaths2 - DAT$Deaths1)
  # DAT$OtherCMPN <- DAT$OtherCMPN + DAT$epi_othercd * (DAT$Deaths2 - DAT$Deaths1)
  
  # Distribute epidemic deaths attributed to all causes pro-rata
  v_idEpi <- which(DAT$epi_colvio + DAT$epi_natdis == 0 & DAT$Deaths2 > DAT$Deaths1)
  if(length(v_idEpi) > 0){
    # Using crisis-included envelope, convert deaths to CSMFs
    DAT[v_idEpi, paste(v_cod)] <- DAT[v_idEpi, paste(v_cod)] / DAT$Deaths2[v_idEpi]
    # These CSMFs will not add up to 1 because the all-cause epidemic deaths were not included in the numerator. 
    # Normalize the CSMFs so they add up to 1.
    DAT[v_idEpi, paste(v_cod)] <- DAT[v_idEpi, paste(v_cod)] / rowSums(DAT[v_idEpi, paste(v_cod)], na.rm = T)
    # Convert fractions back to deaths using crisis-included envelope
    DAT[v_idEpi, paste(v_cod)] <- DAT[v_idEpi, paste(v_cod)] * DAT$Deaths2[v_idEpi]
  }
  return(DAT)
  
}

##################################################
####
####   Squeeze epidemic measles
####
##################################################


fn_add_measepi <- function(DAT){
  
  ## DAT         CSMFs data frame that has been prepared for squeezing
  
  # Adjust epidemic measles so that total measles is not smaller than 0
  v_idMeasles <- which(DAT$meas_epi < 0 & (DAT$Measles + DAT$meas_epi) < 0) 
  if(length(v_idMeasles) > 0){
    # Recode epidemic measles as the negative of endemic measles
    DAT$meas_epi[v_idMeasles] <- -DAT$Measles[v_idMeasles]
  }
  
  # For all country-years with epidemic measles
  v_idMeasles <- which(DAT$meas_epi != 0)
  if (length(v_idMeasles) > 0) {
    
    # Recover denominator from crisis-included deaths and rates
    v_px <- DAT$Deaths2[v_idMeasles]/DAT$Rate2[v_idMeasles]
    
    # Combine endemic and epidemic measles deaths
    # Due to the adjustment above, the lowest value of total measles will be 0
    DAT$Measles[v_idMeasles] <- (DAT$Measles + DAT$meas_epi)[v_idMeasles]
    
    # Add epidemic measles to the top of the crisis-included envelope
    DAT$Deaths2[v_idMeasles] <- (DAT$Deaths2 + DAT$meas_epi)[v_idMeasles]  
    
    # Recalculate the crisis-included mortality rates
    DAT$Rate2[v_idMeasles] <- DAT$Deaths2[v_idMeasles] / v_px
    
  }
  
  return(DAT)
}

##################################################
####
####   Format squeezed output
####
##################################################

fn_format_sqz_output <- function(DAT, DAT_CHN, DAT_NOTSQZ){
  
  ## DAT         CSMFs data frame that has been processed with the above squeezing functions
  ## DAT_CHN     CSMFs data frame for China that has been processed by a subset of the above squeezing functions
  ## DAT_NOTSQZ  CSMFs data frame from prediction (has not been squeezed)

  v_cod <- unique(key_cod$Reclass)  # Vector with ALL CAUSES OF DEATH (including single-cause estimates)
  v_cod <- v_cod[!v_cod %in% c("Other", "Undetermined")]
  
  # Combine China with HMM/LMM countries
  DAT <- bind_rows(DAT, DATCHN)
  
  # Back-transform deaths into fractions
  DAT[, paste(v_cod)] <- DAT[, paste(v_cod)]/DAT$Deaths2
  
  # Checks
  if(any(is.na(rowSums(DAT[, paste(v_cod)])))){
    stop("CSMFs contain NA")
  }
  if(any(round(rowSums(DAT[, paste(v_cod)]),5) != 1)){
    warning("CSMFs do not add up to 1")
  }
  # print(round(rowSums(DAT[, paste(v_cod)]),7), digits = 20)
  #table(rowSums(DAT[, paste(v_cod)]))
  #table(round(rowSums(DAT[, paste(v_cod)]),5))
  #DAT[which(round(rowSums(DAT[, paste(v_cod)]),5) == 0.99942),]
  #DAT[which(rowSums(DAT[, paste(v_cod)]) != 1),]
  #foo <- DAT[which(rowSums(DAT[, paste(v_cod)]) > 1.18),]
  #foo[, c("ISO3",paste(v_cod))]
  
  # Select columns of interest
  DAT <- DAT[, c("ISO3", "Year", "Sex", "Deaths2", "Rate2", paste(v_cod))]
  
  # Incorporate country-year with 0 crisis-free deaths
  # These were excluded in fn_prepare_sqz()
  dat_noDeaths <- DAT_NOTSQZ[which(DAT_NOTSQZ$Deaths1 == 0), ]
  if (nrow(dat_noDeaths) > 0) {
    dat_noDeaths <- dat_noDeaths[, names(DAT)]
    dat_noDeaths[, paste(v_cod)] <- 0
    DAT <- rbind(DAT, dat_noDeaths)  
  }
  
  # Tidy up
  names(DAT)[names(DAT) == "Deaths2"] <- "Deaths"
  names(DAT)[names(DAT) == "Rate2"] <- "Rate"
  DAT <- DAT[order(DAT$ISO3, DAT$Year, DAT$Sex), ]
  rownames(DAT) <- NULL
  
  return(DAT)
}


##################################################
####
####   Combine CSMFs that have been squeezed with GOODVR countries
####
##################################################

fn_combine_csmf <- function(DAT_SQZ, DAT_GOODVR){
  
  ## DAT_SQZ     CSMFs data frame that has been processed with the above squeezing functions
  ## DAT_GOODVR  CSMFs data frame for GOODVR countries (has not been squeezed)
  
  dat <- rbind(DAT_SQZ, DAT_GOODVR)
  
  # Tidy up
  names(dat)[names(dat) == "Deaths2"] <- "Deaths"
  names(dat)[names(dat) == "Rate2"] <- "Rate"
  dat <- dat[order(dat$ISO3, dat$Year, dat$Sex), ]
  rownames(dat) <- NULL
  
  return(dat)
  
}

