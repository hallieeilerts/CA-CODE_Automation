fn_adjustCSMFZeroDeaths <- function(CSMFSQZ, CODALL){
  
  #' @title Adjust CSMF for zero IGME all-cause deaths
  # 
  #' @description When IGME all-cause deaths are equal to zero, recode CSMFs as 0. 
  #' This is an extra step for the intermediate results from the squeezing pipeline.
  #' For the final results from the uncertainty pipeline, it happens within fn_adjustPointIntZeroDeaths (along with a number of other adjustments).
  #
  #' @param CSMF Data frame with CSMFs that have been processed in squeezing pipeline (contains all countries, even those not subject to squeezing).
  #' @param CODALL Vector with CODs for all age groups in correct order.
  #' @return Data frame with all-cause deaths/rates and squeezed CSMFs set to zero when all-cause deaths are zerok as done by fn_adjustPointIntZeroDeaths in the uncertainty pipeline.
  
  dat <- CSMF
  # Causes of death for this age group
  v_cod <- CODALL[CODALL %in% names(dat)]

  # Adjustments to cause-specific fractions/rates/deaths due to zero IGME all-cause deaths
  dat[dat$Deaths2 == 0, v_cod] <- 0
  
  return(dat)
  
}
