fn_sqzOtherCMPNchina <- function(CSMF){
  
  #' @title Squeeze otherCMPN for China DSP
  # 
  #' @description Multiply calculated otherCMPN fraction by crisis-free deaths.
  #' Subtract HIV single-cause deaths, calculate residual otherCMPN deaths.
  #' If residual otherCMPN deaths are less than minimum otherCMPN deaths,
  #' divide otherCMPN deaths by total of HIV single-cause deaths plus minimum otherCMPN deaths.
  #' Use this proportion to scale down HIV single-cause and minimum otherCMPN deaths.
  #' Calculate otherCMPN fraction from scaled down minimum otherCMPN deaths.
  #' Convert scaled down HIV single-cause deaths to fraction.
  #' 
  #' @param CSMF Data frame with CSMFs that has been prepared for squeezing.
  #' @return Data frame where CSMFs have been adjusted for otherCMPN squeezing.
  
  dat <- CSMF
  
  # Multiply othercmpn fraction by envelope, subtract HIV deaths to get residual othercmpn deaths
  dat$OCDresid <- (dat$OtherCMPN * dat$Deaths1) - dat$HIV
  
  # Identify country/years where residual othercmpn deaths are lower than min threshold
  # Will need to squeeze the single causes for these country/years
  v_idSqz <- which(dat$OCDresid < dat$minCD)
  
  # Divide total othercmpn deaths by sum of (HIV, minCD)
  # The latter quantities need to fit into othercmpn
  # The quotient is how much they must be scaled down to do so
  if (length(v_idSqz) > 0) {
    v_scalingFactor <- (dat$OtherCMPN * dat$Deaths1)[v_idSqz] / (dat$HIV + dat$minCD)[v_idSqz]
    # Scale deaths
    dat$HIV[v_idSqz] <- dat$HIV[v_idSqz] * v_scalingFactor
    dat$OCDresid[v_idSqz] <- dat$minCD[v_idSqz] * v_scalingFactor
  }
  
  # Convert to fractions
  # If there are zero crisis-free deaths, recode fraction as zero
  dat$HIV <- dat$HIV/dat$Deaths1
  dat$HIV[is.na(dat$HIV)] <- 0
  dat$OtherCMPN <- dat$OCDresid/dat$Deaths1
  dat$OtherCMPN[is.na(dat$OtherCMPN)] <- 0
  
  return(dat)
  
}
