
fn_prepare_sqz <- function(DAT, ENV, DTH_TB, DTH_HIV, DTH_CRISIS, DTH_MEAS, MINCD, MINLRI){
  
  #' @title Prepare predicted CSMFs for squeezing
  # 
  #' @description Merges single cause data, envelopes, and minimum fractions onto predicted CSMFs data frame.
  #
  #' @param DAT Data frame with predicted CSMFs
  #' @param ENV Data frame age-specific IGME envelopes for crisis-free and crisis-included deaths and rates.
  #' @param DTH_TB Formatted single cause data for TB.
  #' @param DTH_HIV Formatted single cause data for HIV.
  #' @param DTH_CRISIS Formatted single cause data for crisis.
  #' @param DTH_MEAS Formatted single cause data for measles.
  #' @param MINCD Integer with minimum fraction of communicable disease.
  #' @param MINLRI Integer with minimum fraction of LRI.
  #' @return Data frame with predicted CSMFs, single cause data, envelopes, and minimum fractions.
  
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
  # PATCH 2023.02.23
  # Adjust epi deaths to envelopes: All countries
  v_idEpi <- which(dat$epi_colvio + dat$epi_natdis > 0 & dat$Deaths2 == dat$Deaths1)
  if (length(v_idEpi) > 0) {
    dat$epi_colvio[v_idEpi] <- 0
    dat$epi_natdis[v_idEpi] <- 0
  }
  # END PATCH
  #------------------------#
  
  # Exclude country-years with no deaths
  dat <- dat[which(dat$Deaths1 > 0), ]
  # These will be added back in fn_format_sqz_output()
  
  # Tidy up
  rownames(dat) <- NULL
  
  return(dat)
}

fn_prepare_sqz_china <- function(DAT, ENV, DTH_HIV, DTH_CRISIS, MINCD){
  
  #' @title Prepare calculated CSMFs for China DSP for squeezing
  # 
  #' @description Merges single cause data, envelopes, and minimum fractions (converts to deaths) onto calculated CSMFs data frame for China DSP.
  #
  #' @param DAT Data frame with calculated CSMFs for China DSP
  #' @param ENV Data frame age-specific IGME envelopes for crisis-free and crisis-included deaths and rates.
  #' @param DTH_HIV Formatted single cause data for HIV.
  #' @param DTH_CRISIS Formatted single cause data for crisis.
  #' @param MINCD Integer with minimum fraction of communicable disease.
  #' @return Data frame with calculated CSMFs, single cause data, envelopes, and minimum fractions.
  
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
  # PATCH 2023.02.23 
  # Adjust epi deaths to envelopes: China
  v_idEpi <- which(dat$epi_colvio + dat$epi_natdis > 0 & dat$Deaths2 == dat$Deaths1)
  if (length(v_idEpi) > 0) {
    dat$epi_colvio[v_idEpi] <- 0
    dat$epi_natdis[v_idEpi] <- 0
  }
  # END PATCH
  #------------------------#
  
  return(dat)
 
}

fn_sqz_othercmpn <- function(DAT){
  
  #' @title Squeeze otherCMPN
  # 
  #' @description Multiply predicted otherCMPN fraction by crisis-free deaths.
  #' Subtract relevant single-cause deaths, calculate residual otherCMPN deaths.
  #' If residual otherCMPN deaths are less than minimum otherCMPN deaths,
  #' divide otherCMPN deaths by total of relevant single-cause deaths plus minimum otherCMPN deaths.
  #' Use this proportion to scale down relevant single-cause and minimum otherCMPN deaths.
  #' Calculate otherCMPN fraction from scaled down minimum otherCMPN deaths.
  #' Convert scaled down single-cause deaths to fractions.
  #' 
  #' @param DAT Data frame with CSMFs that has been prepared for squeezing.
  #' @return Data frame where CSMFs have been adjusted for otherCMPN squeezing.
  
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

fn_sqz_othercmpn_china <- function(DAT){
  
  #' @title Squeeze otherCMPN for China DSP
  # 
  #' @description Multiply calculated otherCMPN fraction by crisis-free deaths.
  #' Subtract HIV single-cause deaths, calculate residual otherCMPN deaths.
  #' If residual otherCMPN deaths are less than minimum otherCMPN deaths,
  #' divide otherCMPN deaths by total of HIV single-cause deaths plus minimum otherCMPN deaths.
  #' Use this proportion to scale down HIV single-cause and minimum otherCMPN deaths.
  #' Calculate otherCMPN fraction from scaled down minimum otherCMPN deaths.
  #' Convert scaled down HIV single-cause deaths to fraction.
  #' 
  #' @param DAT Data frame with CSMFs that has been prepared for squeezing.
  #' @return Data frame where CSMFs have been adjusted for otherCMPN squeezing.
  
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

fn_sqz_lri <- function(DAT){
  
  #' @title Squeeze LRI
  # 
  #' @description Multiply predicted LRI fraction by crisis-free deaths.
  #' Subtract TBre single-cause deaths, calculate residual LRI deaths.
  #' If residual LRI deaths are less than minimum LRI deaths,
  #' divide LRI deaths by total of TBre single-cause deaths plus minimum LRI deaths.
  #' Use this proportion to scale down TBre single-cause and minimum LRI deaths.
  #' Calculate LRI fraction from scaled down minimum LRI deaths.
  #' Convert scaled down TBre single-cause deaths to fractions.
  #' Sum fractions for TB and TBre to get total TB fraction.
  #' 
  #' @param DAT Data frame with CSMFs that has been prepared for squeezing.
  #' @return Data frame where CSMFs have been adjusted for LRI squeezing.

  # Multiply lri fraction by envelope, subtract TBre deaths to get residual lri deaths
  DAT$LRIresid <- (DAT$LRI * DAT$Deaths1) - DAT$TBre
  
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

fn_sqz_crisisend <- function(DAT){
  
  #' @title Squeeze endemic crisis deaths
  # 
  #' @description Sum crisis-free deaths with endemic crisis single cause deaths.
  #' Calculate fractions for endemic crisis single cause deaths from this sum.
  #' 
  #' Subtract endemic crisis fractions from 1, squeeze other fractions into remaining space.
  #' @param DAT Data frame with CSMFs that has been prepared for squeezing.
  #' @return Data frame where CSMFs have been adjusted for endemic crisis squeezing.
  
  # Vector with all causes of death (including single-cause estimates)
  v_cod <- unique(key_cod$Reclass)  
  v_cod <- v_cod[!v_cod %in% c("Other", "Undetermined")]
  
  # Add crisis-free deaths with endemic CollectVio and NatDis
  v_deaths <- DAT$Deaths1 + DAT$CollectVio + DAT$NatDis
  
  # Calculate fraction of endemic collective violence (Pro-rata squeeze)
  DAT$CollectVio <- DAT$CollectVio/v_deaths
  
  # Calculate fraction of endemic natural disaster (Pro-rata squeeze)
  DAT$NatDis <- DAT$NatDis/v_deaths
  
  # Squeeze other causes into remaining fraction
  DAT[, paste(v_cod[which(!v_cod %in% c("CollectVio", "NatDis"))])] <- 
    DAT[, paste(v_cod[which(!v_cod %in% c("CollectVio", "NatDis"))])] * (1 - DAT$CollectVio - DAT$NatDis)
  
  return(DAT)

}

fn_sqz_crisisepi <- function(DAT){
  
  #' @title Squeeze epidemic crisis deaths
  # 
  #' @description Transform fractions to deaths by multiplying by crisis-free deaths.
  #' Convert single-cause epi_colvio and epi_natdis deaths into proportions.
  #' Multiply epi_colvio and epi_natdis proportions by difference between crisis-free and crisis-included envelopes.
  #' Add deaths to CollectVio and NatDis, respectively.
  #' If there were no epi_colvio/epi_natdis deaths but there were epi_allcause deaths, !!! shouldn't this happen anytime there are epi_allcause, regardless of whether there are epi_colvio/epi_natdis?
  #' divide all deaths by crisis-included envelope,
  #' normalize (distributing epi_allcause pro-rata),
  #' and convert back to deaths.
  #' 
  #' @param DAT Data frame with CSMFs that has been prepared for squeezing.
  #' @return Data frame whith deaths for all causes have been adjusted for epidemic crisis squeezing.

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

fn_add_measepi <- function(DAT){
  
  #' @title Squeeze epidemic measles
  # 
  #' @description If epidemic measles is negative and epidemic plus endemic measles is negative, 
  #' recode epidemic measles as the negative of endemic measles.
  #' For all country years with epidemic measles, add epidemic to endemic measles.
  #' Recover population denominator from crisis-included mortality rate.
  #' Add epidemic measles to crisis-included deaths.
  #' Recalculate crisis-included mortality rate with new deaths and population denominators.
  #' 
  #' @param DAT Data frame with deaths that have been squeezed.
  #' @return Data frame with deaths for all causes where measles deaths and 
  #' crisis-included deaths and rates have been updated to include epidemic measles.
  
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

fn_format_sqz_output <- function(DAT, DAT_CHN, DAT_NOTSQZ){
  
  #' @title Format squeezed output
  #' 
  #' @description Combine squeezed CSMFs with country years that were not squeezed due to there being zero crisis-free deaths, 
  #' rename crisis-included deaths and rates columns.
  #'
  #' @param DAT Data frame with predicted CSMFs that have been processed with squeezing functions.
  #' @param DAT_CHN Data frame with CSMFs for China that has been processed by a subset of squeezing functions.
  #' @param DAT_NOTSQZ Data frame with predicted CSMFs, prior to squeezing.
  #' @return Data frame with predicted CSMFs, single cause data, envelopes, and minimum fractions.
  
  v_cod <- unique(key_cod$Reclass)  # Vector with ALL CAUSES OF DEATH (including single-cause estimates)
  v_cod <- v_cod[!v_cod %in% c("Other", "Undetermined")]
  
  # Combine China with HMM/LMM countries
  # Add empty columns to DAT_CHN
  DAT_CHN[setdiff(names(DAT), names(DAT_CHN))] <- NA
  DAT <- dplyr::bind_rows(DAT, DAT_CHN)
  
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

fn_combine_csmf <- function(DAT_SQZ, DAT_GOODVR){
  
  #' @title Combine CSMFs that have been squeezed with GOODVR countries
  # 
  #' @description Combine data frames, rename crisis-included deaths and rates columns.
  #
  #' @param DAT_SQZ Data frame with predicted CSMFs that have been processed with squeezing functions.
  #' @param DAT_GOODVR Data frame with CSMFs for GOODVR countries that has not been squeezed.
  #' @return Data frame with predicted CSMFs, single cause data, envelopes, and minimum fractions.
  
  dat <- rbind(DAT_SQZ, DAT_GOODVR)
  
  # Tidy up
  names(dat)[names(dat) == "Deaths2"] <- "Deaths"
  names(dat)[names(dat) == "Rate2"] <- "Rate"
  dat <- dat[order(dat$ISO3, dat$Year, dat$Sex), ]
  rownames(dat) <- NULL
  
  return(dat)
  
}

