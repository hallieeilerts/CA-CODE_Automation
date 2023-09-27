fn_formatDraws <- function(L_CSMFDRAWS_HMM, L_CSMFDRAWS_LMM){
  
  #' @title Format draws of predicted fractions
  # 
  #' @description Combines randomly sampled draws for HMM and LMM.
  #' 
  #' @param L_CSMFDRAWS_HMM List of length number of draws.
  #' Each list element is a data frame with CSMFs for all years being predicted for HMM countries.
  #' @param L_CSMFDRAWS_LMM List of length number of draws.
  #' Each list element is a data frame with CSMFs for all years being predicted for LMM countries.
  #' @return List of length number of draws.
  #' Each list element is a data frame with predicted CSMFs for HMM and LMM for one draw.
  
  l_draws <- mapply(rbind, L_CSMFDRAWS_HMM, L_CSMFDRAWS_LMM, SIMPLIFY=FALSE)
  
  # Rearrange columns
  l_draws <- lapply(l_draws, function(x){ x[, c(idVars, sort(names(x)[which(!names(x) %in% idVars)]))] })
  
  # Tidy up
  l_draws <- lapply(l_draws, function(x){ x[order(x$ISO3, x$Year),] })
  
  return(l_draws)
  
}