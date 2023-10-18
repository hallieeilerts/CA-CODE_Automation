fn_sqzLRI <- function(CSMF){
  
  #' @title Squeeze LRI
  # 
  #' @description Multiply predicted LRI fraction by crisis-free deaths. Subtract respiratory TB single-cause deaths to get residual LRI deaths. If residual LRI deaths are less than minimum LRI deaths, divide total LRI deaths by the sum of respiratory TB single-cause deaths and minimum LRI deaths. Use this factor to scale down respiratory TB single-cause deaths and minimum LRI deaths. Calculate fractions from scaled respiratory TB and minimum LRI deaths using crisis-free envelope. Sum fractions for TB and respiratory TB to get total TB fraction.
  #' 
  #' @param CSMF Data frame with CSMFs that has been prepared for squeezing.
  #' @return Data frame where CSMFs have been adjusted for LRI squeezing.
  
  dat <- CSMF
  
  # Multiply lri fraction by envelope, subtract TBre deaths to get residual lri deaths
  dat$LRIresid <- (dat$LRI * dat$Deaths1) - dat$TBre
  
  # Identify country/years where residual lri deaths are lower than min threshold
  # Will need to squeeze the single causes for these country/years
  v_idSqz <- which(dat$LRIresid < dat$minLRI)
  
  # Divide total lri deaths by sum of (TBre, minLRI)
  # The latter quantities need to fit into LRI
  # The quotient is how much they must be scaled down to do so
  if(length(v_idSqz) > 0){
    v_scalingFactor <- (dat$LRI * dat$Deaths1)[v_idSqz] / (dat$TBre + dat$minLRI)[v_idSqz]
    # Scale deaths
    dat$TBre[v_idSqz] <- dat$TBre[v_idSqz] * v_scalingFactor
    dat$LRIresid[v_idSqz] <- dat$minLRI[v_idSqz] * v_scalingFactor
    
    # Convert to fractions
    # If there are zero crisis-free deaths, recode fraction as zero
    dat$TBre <- dat$TBre/dat$Deaths1
    dat$TBre[is.na(dat$TBre)] <- 0
    dat$LRI <- dat$LRIresid/dat$Deaths1
    dat$LRI[is.na(dat$LRI)] <- 0
    
    # Final TB fraction
    dat$TB <- apply(dat[, c("TB", "TBre")], 1, sum)
  } 
  
  return(dat)
  
}
