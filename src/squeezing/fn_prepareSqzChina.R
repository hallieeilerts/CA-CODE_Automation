fn_prepareSqzChina <- function(CSMF, DAT_HIV, DAT_CRISIS, FRAC_CD){
  
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
