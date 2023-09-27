
fn_merge_env <- function(CSMF, ENV){
 
  #' @title Merge envelopes onto CSMFs
  # 
  #' @description Merges envelopes onto predicted CSMFs data frame.
  #
  #' @param CSMF Data frame with predicted CSMFs
  #' @param ENV Data frame age-specific IGME envelopes for crisis-free and crisis-included deaths and rates.
  #' @return Data frame with predicted CSMFs and envelopes.
  
  env <- ENV[, names(ENV) %in% c(idVars, "Deaths1", "Rate1", "Deaths2", "Rate2")]
  
  # Merge on IGME envelopes
  dat <- merge(CSMF, env, by = idVars, all.x = T)
  
  return(dat)
   
}

fn_prepare_sqz <- function(CSMF, DAT_TB, DAT_HIV, DAT_CRISIS, DAT_MEAS, FRAC_CD, FRAC_LRI){
  
  #' @title Prepare predicted CSMFs for squeezing
  # 
  #' @description Merges single cause data and minimum fractions onto predicted CSMFs data frame.
  #
  #' @param CSMF Data frame with predicted CSMFs
  #' @param DAT_TB Formatted single cause data for TB.
  #' @param DAT_HIV Formatted single cause data for HIV.
  #' @param DAT_CRISIS Formatted single cause data for crisis.
  #' @param DAT_MEAS Formatted single cause data for measles.
  #' @param FRAC_CD Integer with minimum fraction of communicable disease.
  #' @param FRAC_LRI Integer with minimum fraction of LRI.
  #' @return Data frame with predicted CSMFs, single cause data, and minimum fractions.
  
  # Merge on TB
  dat <- merge(CSMF, DAT_TB, by = idVars, all.x = T)
  
  # Merge on HIV
  dat <- merge(dat, DAT_HIV, by = idVars, all.x = T)
  
  # Merge on measles
  if(!is.null(DAT_MEAS)){
    dat <- merge(dat, DAT_MEAS, by = idVars, all.x = T)
  }
  
  # Merge on crisis
  dat <- merge(dat, DAT_CRISIS, by = idVars, all.x = T)
  
  # Merge on minimum CD fraction and convert to deaths
  dat$minCD <- dat$Deaths1 * FRAC_CD
  
  # Merge on minimum LRI fraction and convert to deaths
  if(!is.null(FRAC_LRI)){dat$minLRI <- dat$Deaths1 * FRAC_LRI}
  
  # Exclude country-years with no deaths
  dat <- dat[which(dat$Deaths1 > 0), ]
  # These will be added back in fn_format_sqz_output()
  
  # Tidy up
  rownames(dat) <- NULL
  
  return(dat)
}

fn_prepare_sqz_china <- function(CSMF, DAT_HIV, DAT_CRISIS, FRAC_CD){
  
  #' @title Prepare calculated CSMFs for China DSP for squeezing
  # 
  #' @description Merges single cause data, envelopes, and minimum fractions (converts to deaths) onto calculated CSMFs data frame for China DSP.
  #
  #' @param CSMF Data frame with calculated CSMFs for China DSP
  #' @param DAT_HIV Formatted single cause data for HIV.
  #' @param DAT_CRISIS Formatted single cause data for crisis.
  #' @param FRAC_CD Integer with minimum fraction of communicable disease.
  #' @return Data frame with calculated CSMFs, single cause data, envelopes, and minimum fractions.
  
  # Merge on HIV
  dat <- merge(CSMF, DAT_HIV, by = idVars, all.x = T)
  
  # Merge on crisis
  dat <- merge(dat, DAT_CRISIS[, names(DAT_CRISIS)[!names(DAT_CRISIS) %in% c("CollectVio", "NatDis")]], 
               by = idVars, all.x = T)
  
  # Merge on minimum CD fraction and convert to deaths
  dat$minCD <- dat$Deaths1 * FRAC_CD
  
  return(dat)
 
}

fn_sqz_othercmpn <- function(CSMF){
  
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
  #' @param CSMF Data frame with CSMFs that has been prepared for squeezing.
  #' @return Data frame where CSMFs have been adjusted for otherCMPN squeezing.
  
  dat <- CSMF
  
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

}

fn_sqz_othercmpn_china <- function(CSMF){
  
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
  #' @param CSMF Data frame with CSMFs that has been prepared for squeezing.
  #' @return Data frame where CSMFs have been adjusted for otherCMPN squeezing.
  
  dat <- CSMF
  
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

}

fn_sqz_lri <- function(CSMF){
  
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
  #' @param CSMF Data frame with CSMFs that has been prepared for squeezing.
  #' @return Data frame where CSMFs have been adjusted for LRI squeezing.

  dat <- CSMF
  
  # Multiply lri fraction by envelope, subtract TBre deaths to get residual lri deaths
  dat$LRIresid <- (dat$LRI * dat$Deaths1) - dat$TBre
  
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

}

fn_sqz_crisisend <- function(CSMF, KEY_COD, UNCERTAINTY = FALSE){
  
  #' @title Squeeze endemic crisis deaths
  # 
  #' @description Sum crisis-free deaths with endemic crisis single cause deaths.
  #' Calculate fractions for endemic crisis single cause deaths from this sum.
  #' 
  #' Subtract endemic crisis fractions from 1, squeeze other fractions into remaining space.
  #' @param CSMF Data frame with CSMFs that has been prepared for squeezing.
  #' @param KEY_COD Data frame with age-specific CODs with different levels of classification.
  #' @param UNCERTAINTY A boolean that denotes whether this function is being run as part of the squeezing pipeline or uncertainty pipeline.
  #' @return Data frame where CSMFs have been adjusted for endemic crisis squeezing.
  
  dat <- CSMF
  
  # Vector with all causes of death (including single-cause estimates)
  v_cod <- unique(KEY_COD$Reclass)  
  v_cod <- v_cod[!v_cod %in% c("Other", "Undetermined")]
  
  if(UNCERTAINTY == FALSE){
    # Add crisis-free deaths with endemic CollectVio and NatDis
    v_deaths <- dat$Deaths1 + dat$CollectVio + dat$NatDis
    
    # Calculate fraction of endemic collective violence (Pro-rata squeeze)
    dat$CollectVio <- dat$CollectVio/v_deaths
    
    # Calculate fraction of endemic natural disaster (Pro-rata squeeze)
    dat$NatDis <- dat$NatDis/v_deaths
    
    #' Note: This code is also included in the uncertainty function fn_rand_assign_crisisend(),
    #' so this if() statement avoids repeating it.
    #' In that function, a multinomial distribution is subsequently used to
    #' randomly sample counts for NatDis/CollectVio deaths with input parameters of the
    #' fractions just calculated and Deaths1 as Total count.
    #' The sampled counts are then divided by Deaths1 to get the final fractions.
  }
  
  # Squeeze other causes into remaining fraction
  dat[, paste(v_cod[which(!v_cod %in% c("CollectVio", "NatDis"))])] <- 
    dat[, paste(v_cod[which(!v_cod %in% c("CollectVio", "NatDis"))])] * (1 - dat$CollectVio - dat$NatDis)
  
  return(dat)

}

fn_sqz_crisisepi <- function(CSMF, KEY_COD){
  
  #' @title Squeeze epidemic crisis deaths
  # 
  #' @description Transform fractions to deaths by multiplying by crisis-free deaths.
  #' Convert single-cause epi_colvio and epi_natdis deaths into proportions.
  #' Multiply epi_colvio and epi_natdis proportions by difference between crisis-free and crisis-included envelopes.
  #' Add deaths to CollectVio and NatDis, respectively.
  #' If there were no epi_colvio/epi_natdis deaths but there is a difference between crisis-free and crisis-included envelopes,
  #' divide all deaths by crisis-included envelope, normalize (distributing epi_allcause pro-rata), and convert back to deaths.
  #' 
  #' @param CSMF Data frame with CSMFs that has been prepared for squeezing.
  #' @param KEY_COD Data frame with age-specific CODs with different levels of classification.
  #' @return Data frame whith deaths for all causes have been adjusted for epidemic crisis squeezing.

  dat <- CSMF
  
  # Vector with all causes of death (including single-cause estimates)
  v_cod <- unique(KEY_COD$Reclass)  
  v_cod <- v_cod[!v_cod %in% c("Other", "Undetermined")]
  
  # Transform fractions into deaths
  dat[, paste(v_cod)] <- dat[, paste(v_cod)] * dat$Deaths1
  
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
    dat[v_idEpi, paste(v_cod)] <- dat[v_idEpi, paste(v_cod)] / dat$Deaths2[v_idEpi]
    # These CSMFs will not add up to 1 because the all-cause epidemic deaths were not included in the numerator. 
    # Normalize the CSMFs so they add up to 1.
    dat[v_idEpi, paste(v_cod)] <- dat[v_idEpi, paste(v_cod)] / rowSums(dat[v_idEpi, paste(v_cod)], na.rm = T)
    # Convert fractions back to deaths using crisis-included envelope
    dat[v_idEpi, paste(v_cod)] <- dat[v_idEpi, paste(v_cod)] * dat$Deaths2[v_idEpi]
  }
  return(dat)
  
}

fn_add_measepi <- function(DTH){
  
  #' @title Squeeze epidemic measles
  # 
  #' @description If epidemic measles is negative and epidemic plus endemic measles is negative, 
  #' recode epidemic measles as the negative of endemic measles.
  #' For all country years with epidemic measles, add epidemic to endemic measles.
  #' Recover population denominator from crisis-included mortality rate.
  #' Add epidemic measles to crisis-included deaths.
  #' Recalculate crisis-included mortality rate with new deaths and population denominators.
  #' 
  #' @param DTH Data frame with deaths that have been squeezed.
  #' @return Data frame with deaths for all causes where measles deaths and all-cause crisis-included deaths and rates have been updated to include epidemic measles.
  
  dat <- DTH
  
  # Adjust epidemic measles so that total measles is not smaller than 0
  v_idMeasles <- which(dat$meas_epi < 0 & (dat$Measles + dat$meas_epi) < 0) 
  if(length(v_idMeasles) > 0){
    # Recode epidemic measles as the negative of endemic measles
    dat$meas_epi[v_idMeasles] <- -dat$Measles[v_idMeasles]
  }
  
  # For all country-years with epidemic measles
  v_idMeasles <- which(dat$meas_epi != 0)
  if (length(v_idMeasles) > 0) {
    
    # Recover denominator from crisis-included deaths and rates
    v_px <- dat$Deaths2[v_idMeasles]/dat$Rate2[v_idMeasles]
    
    # Combine endemic and epidemic measles deaths
    # Due to the adjustment above, the lowest value of total measles will be 0
    dat$Measles[v_idMeasles] <- (dat$Measles + dat$meas_epi)[v_idMeasles]
    
    # Add epidemic measles to the top of the crisis-included envelope
    dat$Deaths2[v_idMeasles] <- (dat$Deaths2 + dat$meas_epi)[v_idMeasles]  
    
    # Recalculate the crisis-included mortality rates
    dat$Rate2[v_idMeasles] <- dat$Deaths2[v_idMeasles] / v_px
    
  }
  
  return(dat)
}

fn_format_sqz_output <- function(DTH, DTH_CHN, CSMF_NOTSQZ, KEY_COD){
  
  #' @title Format squeezed output
  #' 
  #' @description Combine squeezed CSMFs with country years that were not squeezed due to there being zero crisis-free deaths, rename crisis-included deaths and rates columns.
  #'
  #' @param DTH Data frame with predicted deaths that have been processed with squeezing functions.
  #' @param DTH_CHN Data frame with deaths for China that has been processed by a subset of squeezing functions.
  #' @param CSMF_NOTSQZ Data frame with predicted CSMFs (all mortality modeled countries), prior to squeezing.
  #' @param KEY_COD Data frame with age-specific CODs with different levels of classification.
  #' @return Data frame with CSMFs that have been processed by squeezing functions, all-cause crisis-free and crisis-included deaths and rates.
  
  dth <- DTH
  dth_CHN <- DTH_CHN
  csmf_notsqz <- CSMF_NOTSQZ
  
  v_cod <- unique(KEY_COD$Reclass)  # Vector with ALL CAUSES OF DEATH (including single-cause estimates)
  v_cod <- v_cod[!v_cod %in% c("Other", "Undetermined")]
  
  # Combine China with HMM/LMM countries
  # Add empty columns to dth_chn
  dth_CHN[setdiff(names(dth), names(dth_CHN))] <- NA
  dat <- rbind(dth, dth_CHN)
  
  # Back-transform deaths into fractions
  dat[, paste(v_cod)] <- dat[, paste(v_cod)]/dat$Deaths2
  
  # Select columns of interest
  v_cols <- c(idVars, "Deaths1", "Rate1", "Deaths2", "Rate2", v_cod)
  v_cols <- v_cols[v_cols %in% names(dat)]
  dat <- dat[, paste(v_cols)]
  
  # Incorporate country-year with 0 crisis-free deaths
  # These were excluded in fn_prepare_sqz()
  dat_noDeaths <- csmf_notsqz[which(csmf_notsqz$Deaths1 == 0), ]
  if (nrow(dat_noDeaths) > 0) {
    v_cols <- c(idVars, "Deaths1", "Rate1", "Deaths2", "Rate2", v_cod)
    v_cols <- v_cols[v_cols %in% names(dat_noDeaths)]
    dat_noDeaths <- dat_noDeaths[, paste(v_cols)]
    # Add zeros for CSMFs in those years
    dat_noDeaths[setdiff(names(dat), names(dat_noDeaths))] <- 0
    dat <- rbind(dat, dat_noDeaths)  
  }
  
  # Tidy up
  dat <- dat[order(dat$ISO3, dat$Year, dat$Sex), ]
  rownames(dat) <- NULL
  
  return(dat)
}

fn_check_csmf_sqz <- function(CSMF, KEY_COD){
  
  #' @title Check if squeezed CSMFs add up to 1 or contain NAs
  # 
  #' @description Checks for country-years where squeezed fractions do not add up to 1.
  #
  #' @param CSMF Data frame with CSMFs that have been processed by squeezing functions, all-cause crisis-free and crisis-included deaths and rates.
  #' @param KEY_COD Data frame with age-specific CODs with different levels of classification.
  #' @return Data frame with rows where fractions for country-year do not add up to 1 or contain an NA.
  
  dat <- CSMF
  
  v_cod <- unique(KEY_COD$Reclass)  # Vector with ALL CAUSES OF DEATH (including single-cause estimates)
  v_cod <- v_cod[!v_cod %in% c("Other", "Undetermined")]
  
  v_containsNA <- which(is.na(rowSums(dat[, paste(v_cod)])))
  v_sumnot1    <- which(round(rowSums(dat[, paste(v_cod)]),5) != 1)
  v_audit      <- unique(v_containsNA, v_sumnot1)
  v_audit      <- sort(v_audit)
  
  # Checks
  if(any(is.na(rowSums(dat[, paste(v_cod)])))){
    warning("CSMFs contain NA")
  }
  if(any(round(rowSums(dat[, paste(v_cod)]),5) != 1)){
    warning("CSMFs do not add up to 1")
  }
  #print(round(rowSums(DAT[, paste(v_cod)]),7), digits = 20)
  #table(rowSums(DAT[, paste(v_cod)]))
  #table(round(rowSums(DAT[, paste(v_cod)]),5))
  #DAT[which(round(rowSums(DAT[, paste(v_cod)]),5) == 0.99942),]
  #DAT[which(rowSums(DAT[, paste(v_cod)]) != 1),]
  #foo <- DAT[which(rowSums(DAT[, paste(v_cod)]) > 1.18),]
  #foo[, c("ISO3",paste(v_cod))]
  
  dat <- dat[c(v_audit),]
  dat$csmf_SUM <- round(rowSums(dat[, paste(v_cod)]),5)
  rownames(dat) <- NULL
  
  return(dat)
}

fn_format_all_output <- function(CSMF_ALL, KEY_COD){
  
  #' @title Combine CSMFs that have been squeezed with GOODVR countries
  # 
  #' @description Combine data frames, rename crisis-included deaths and rates columns.
  #
  #' @param CSMF_ALL Data frame with CSMFs that have been processed by squeezing functions and CSMFs for GOODVR countries that have not been squeezed.
  #' @param KEY_COD Data frame with age-specific CODs with different levels of classification.
  #' @return Data frame with CSMFs that have been processed by squeezing functions, CSMFs that were not squeezed (GOODVR), all-cause crisis-free and crisis-included deaths and rates.
  
  dat <- CSMF_ALL
  v_cod <- unique(KEY_COD$Reclass)  # Vector with ALL CAUSES OF DEATH (including single-cause estimates)
  v_cod <- v_cod[!v_cod %in% c("Other", "Undetermined")]
  
  # Arrange columns
  v_cols <- c(idVars, "Deaths1", "Rate1", "Deaths2", "Rate2", v_cod)
  v_cols <- v_cols[v_cols %in% names(dat)]
  dat <- dat[, paste(v_cols)]
  
  # Tidy up
  dat <- dat[order(dat$ISO3, dat$Year, dat$Sex), ]
  rownames(dat) <- NULL
  
  return(dat)
  
}

fn_calc_region <- function(CSMF, CODALL, KEY_REGION){
  
  #' @title Calculate CSMFs for global regions
  # 
  #' @description Converts CSMFs to deaths, back calculates denominator, aggregates deaths and population counts by region, recalculates CSMFs and all-cause mortality rate.
  #
  #' @param CSMF Data frame with CSMFs that have been processed by squeezing functions, CSMFs that were not squeezed (GOODVR), all-cause crisis-free and crisis-included deaths and rates.
  #' @param KEY_COD Data frame with age-specific CODs with different levels of classification.
  #' @param KEY_REGION Data frame with countries and different regional classifications.
  #' @return Data frame with regional CSMFs, all-cause crisis-included deaths and rates.
  
  # Merge on regions
  dat <- merge(CSMF, KEY_REGION, by = "ISO3")
  
  # Causes of death for this age group
  v_cod <- CODALL[CODALL %in% names(dat)]
  #v_cod <- unique(KEY_COD$Reclass)  # Vector with ALL CAUSES OF DEATH (including single-cause estimates)
  #v_cod <- v_cod[!v_cod %in% c("Other", "Undetermined")]
  
  # Create unified variable for region
  dat$Region <- dat$UNICEFReportRegion1
  # If report region 2 is not missing, use it instead
  dat$Region[which(dat$UNICEFReportRegion2 != "")] <- dat$UNICEFReportRegion2[which(dat$UNICEFReportRegion2 != "")]
  
  # Manually add extra regions
  df_world <- dat
  df_world$Region <- "World"
  df_eca <- subset(dat, UNICEFReportRegion1 == "Europe and central Asia")
  df_eca$Region <- "Europe and central Asia"
  df_ssa <- subset(dat, UNICEFReportRegion1 == "Sub-Saharan Africa")
  df_ssa$Region <- "Sub-Saharan Africa"
  dat <- rbind(dat, df_world, df_eca, df_ssa)
  
  # Create list of regions, move world to front
  v_regions <- sort(unique(dat$Region))
  v_regions <- v_regions[c(which(v_regions == "World"), which(v_regions != "World"))]
  
  # Convert CSMFs to deaths
  dat[, paste(v_cod)] <- dat[, paste(v_cod)] * dat$Deaths2
  
  # Back calculate denominator from deaths and mortality rate
  dat$Px <- dat$Deaths2/dat$Rate2
  
  # Aggregate countries for each region
  dat <- ddply(dat, ~Region, function(x){aggregate(x[, c("Deaths2", "Px", paste(v_cod))], 
                                                   by = list(x$Year, x$Sex), sum, na.rm = T)})
  names(dat)[1:3] <- c("Region", "Year", "Sex")
  
  # Re-calculate CSMFs
  dat[, paste(v_cod)] <- dat[, paste(v_cod)] / dat$Deaths2
  dat[, paste(v_cod)] <- dat[, paste(v_cod)] / rowSums(dat[, paste(v_cod)])
  
  # Re-calculate mortality rate
  dat$Rate2 <- dat$Deaths2 / dat$Px
  # Remove denominator
  dat <- dat[, names(dat) != "Px"]
  
  # Order columns
  dat <- dat[, c("Region", "Year","Sex", "Deaths2", "Rate2", v_cod)]
  
  # Tidy up
  dat$Region <- factor(dat$Region, levels = v_regions, ordered = TRUE)
  dat <- dat[order(dat$Region, dat$Year, dat$Sex), ]
  dat$Region <- as.character(dat$Region)
  rownames(dat) <- NULL
  
  return(dat)
  
}


