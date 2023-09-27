fn_prepareSqz <- function(CSMF, DAT_TB, DAT_HIV, DAT_CRISIS, DAT_MEAS, FRAC_CD, FRAC_LRI){
  
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
