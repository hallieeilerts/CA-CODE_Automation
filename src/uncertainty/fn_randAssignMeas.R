fn_randAssignMeas <- function(CSMFDRAW){
  
  #' @title Randomly assign measles deaths for current draw
  # 
  #' @description Cap total measles deaths (endemic + epidemic) to crisis-included envelope.
  #' If total measles deaths > 0, sample new total from log normal distribution.
  #' If endemic measles deaths > 0, recalculate endemic by subtracting original epidemic measles from randomly sampled total.
  #' If epidemic measles deaths > 0, recalculate epidemic by subtracting original endemic measles from randomly sampled total.
  #' 
  #' @param CSMFDRAW Data frame that is one draw of predicted CSMFs, single cause data, envelopes, and minimum fractions.
  #' @return Data frame with randomly sampled measles deaths.
  
  dat <- CSMFDRAW
  
  # Cap the upper bound of Measles (EPI + END) to the envelope
  dat$msl_ub[which(dat$msl_ub > dat$Deaths2)] <- dat$Deaths2[which(dat$msl_ub > dat$Deaths2)]
  
  # Uncertainty intervals only available for endemic + epidemic Measles
  # Random epidemic + endemic Measles
  
  # Randomly sample value for total measles
  v_msl <- rep(0, nrow(dat))
  v_idSqz <- which(dat$msl != 0)
  if(length(v_idSqz) > 0){
    # Sample from log normal distribution
    v_msl[v_idSqz] <- fn_sampleLogNorm(MU = dat$msl[v_idSqz],
                                         LB = dat$msl_lb[v_idSqz],
                                         UB = dat$msl_ub[v_idSqz])
    # Save point estimate for endemic measles
    v_meas_end <- dat$Measles
    v_idSqz <- which(dat$Measles != 0)
    # Calculate random endemic measles by subtracting epidemic from random total
    dat$Measles[v_idSqz] <- v_msl[v_idSqz] - dat$meas_epi[v_idSqz] 
    # Avoid negative values
    dat$Measles[which(dat$Measles < 0)] <- 0
    # Calculate random epidemic measles by subtracting endemic point estimate from random total
    v_idSqz <- which(dat$meas_epi != 0)
    dat$meas_epi[v_idSqz] <- v_msl[v_idSqz] - v_meas_end[v_idSqz]
    # Note: Measles and meas_epi do not need to add up to msl, but their total should be within msl_lb and msl_ub
    ## meas_epi will be added on top of envelope
  }
  
  return(dat)
  
}