fn_addMeasEpi <- function(DTH){
  
  #' @title Squeeze epidemic measles
  # 
  #' @description If epidemic measles is negative and epidemic plus endemic measles is negative, recode epidemic measles as the negative of endemic measles. For all country years with negative or positive epidemic measles, add epidemic to endemic measles. Back-calculate population denominator from crisis-included mortality rate and add epidemic measles to crisis-included deaths. Recalculate crisis-included mortality rate with new deaths and population denominators.
  #' 
  #' @param DTH Data frame with deaths that have been squeezed.
  #' @return Data frame where epidemic measles deaths have been incorporated, and all-cause crisis-included deaths and rates have been updated to reflect new total number of deaths.
  
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