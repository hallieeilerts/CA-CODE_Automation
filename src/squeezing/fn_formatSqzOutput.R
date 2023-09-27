fn_formatSqzOutput <- function(DTH, DTH_CHN, CSMF_NOTSQZ, KEY_COD){
  
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