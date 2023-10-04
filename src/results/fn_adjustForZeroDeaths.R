fn_adjustForZeroDeaths <- function(CSMF, CODALL){
  
  #' @title Adjust CSMF for zero IGME all-cause deaths
  # 
  #' @description When IGME all-cause deaths are equal to zero, recode CSMFs as 0. 
  #' This is an extra step for the intermediate results from the squeezing pipeline.
  #' It happens within fn_adjustPointInt (along with a number of other adjustments) for the final results from the uncertainty pipeline.
  #
  #' @param CSMF Data frame with 
  #' @param CODALL 
  #' @return 
  
  dat <- CSMF
  # Causes of death for this age group
  v_cod <- CODALL[CODALL %in% names(dat)]
  #v_cod <- unique(KEY_COD$Reclass)  # Vector with ALL CAUSES OF DEATH (including single-cause estimates)
  #v_cod <- v_cod[!v_cod %in% c("Other", "Undetermined")]

  # Adjustments to cause-specific fractions/rates/deaths due to zero IGME all-cause deaths
  dat[dat$Deaths2 == 0, v_cod] <- 0
  
  return(dat)
  
}
