fn_mergeEnv <- function(CSMF, ENV){
  
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
