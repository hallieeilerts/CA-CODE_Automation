fn_formatAllOutput <- function(CSMF_ALL, KEY_COD){
  
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