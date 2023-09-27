fn_formatPrediction <- function(L_CSMF_HMM, L_CSMF_LMM){
  
  #' @title Format predicted CSMFs
  # 
  #' @description Combines predicted CSMFs for HMM and LMM countries.
  #
  #' @param L_CSMF_HMM  List of length number of years being predicted. Each list element is a dataframe with predicted CSMFs for HMM.
  #' @param L_CSMF_LMM List of length number of years being predicted. Each list element is a dataframe with predicted CSMFs for HMM.
  #' @return Dataframe with predicted CSMFs for HMM and LMM.
  
  df_HMM <- do.call(rbind, L_CSMF_HMM)
  df_LMM <- do.call(rbind, L_CSMF_LMM)
  dat <- rbind(df_HMM, df_LMM)
  
  # Rearrange columns
  dat <- dat[, c(idVars, sort(names(dat)[which(!names(dat) %in% idVars)]))] 
  
  # Tidy up
  dat <- dat[order(dat$ISO3, dat$Year),]
  
  return(dat)
  
}
