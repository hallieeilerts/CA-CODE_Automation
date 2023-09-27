fn_randAssignTB <- function(CSMFDRAW){
  
  #' @title Randomly assign TB deaths for current draw
  # 
  #' @description Cap TB deaths (TB and TBre) to crisis-included envelope.
  #' If TB deaths != 0 and is within lower and upper TB bounds, sample from log normal distribution.
  #' 
  #' @param CSMFDRAW Data frame that is one draw of predicted CSMFs, single cause data, envelopes, and minimum fractions.
  #' @return Data frame with randomly sampled TB deaths.
  
  dat <- CSMFDRAW
  
  # Cap the upper bound of TB to the envelope
  dat$tb_ub[which(dat$tb_ub > dat$Deaths1)] <- dat$Deaths1[which(dat$tb_ub > dat$Deaths1)]
  # Values to be squeezed
  v_idSqz <- which(dat$TB != 0 & dat$TB > dat$tb_lb & dat$TB < dat$tb_ub)
  # Sample random values
  if (length(v_idSqz) > 0) {
    dat$TB[v_idSqz] <- fn_sampleLogNorm(MU = dat$TB[v_idSqz],
                                          LB = dat$tb_lb[v_idSqz],
                                          UB = dat$tb_ub[v_idSqz])
  }
  
  if(respTB){
    
    # Cap the upper bound of TB to the envelope
    dat$tbre_ub[which(dat$tbre_ub > dat$Deaths1)] <- dat$Deaths1[which(dat$tbre_ub > dat$Deaths1)]
    # Values to be squeezed
    v_idSqz <- which(dat$TBre != 0 & dat$TBre > dat$tbre_lb & dat$TBre < dat$tbre_ub)
    # Sample random values
    if (length(v_idSqz) > 0) {
      dat$TBre[v_idSqz] <- fn_sampleLogNorm(MU = dat$TBre[v_idSqz],
                                              LB = dat$tbre_lb[v_idSqz],
                                              UB = dat$tbre_ub[v_idSqz])
    }
  }
  
  return(dat)
}
