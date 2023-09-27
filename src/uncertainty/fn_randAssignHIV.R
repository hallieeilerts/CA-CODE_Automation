fn_randAssignHIV <- function(CSMFDRAW){
  
  #' @title Randomly assign HIV deaths for current draw
  # 
  #' @description If HIV deaths != 0 and is within lower and upper HIV bounds, sample from log normal distribution.
  #' 
  #' @param CSMFDRAW Data frame that is one draw of predicted CSMFs, single cause data, envelopes, and minimum fractions.
  #' @return Data frame with randomly sampled HIV deaths.
  
  dat <- CSMFDRAW
  
  # Values to be squeezed
  v_idSqz <- which(dat$HIV != 0 & dat$HIV > dat$hiv_lb & dat$HIV < dat$hiv_ub)
  
  # Sample random values
  if (length(v_idSqz) > 0) {
    dat$HIV[v_idSqz] <- rtnorm(n = length(v_idSqz), mean = dat$HIV[v_idSqz],
                               sd = (dat$hiv_ub[v_idSqz] - dat$hiv_lb[v_idSqz]) / 3.92,
                               lower = 0)
  }
  
  return(dat)
}
