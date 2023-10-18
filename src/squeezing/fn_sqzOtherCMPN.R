fn_sqzOtherCMPN <- function(CSMF){
  
  #' @title Squeeze otherCMPN
  # 
  #' @description Multiply predicted otherCMPN fraction by crisis-free deaths. Subtract relevant single-cause deaths (TB, HIV, measles), calculate residual otherCMPN deaths. If residual otherCMPN deaths are less than minimum otherCMPN deaths, divide otherCMPN deaths by total of single-cause deaths plus minimum otherCMPN deaths. Use this factor to scale down single-cause deaths and minimum otherCMPN deaths. Calculate fractions from scaled single-causes and minimum otherCMPN deaths using crisis-free envelope.
  #' 
  #' @param CSMF Data frame with CSMFs that has been prepared for squeezing.
  #' @return Data frame where CSMFs have been adjusted for otherCMPN squeezing.
  
  dat <- CSMF
  
  # Multiply othercmpn fraction by envelope, subtract (TB, HIV, Measles) deaths to get residual othercmpn deaths
  if("Measles" %in% names(dat)){
    dat$OCDresid <- (dat$OtherCMPN * dat$Deaths1) - dat$TB - dat$HIV - dat$Measles
  }else{
    dat$OCDresid <- (dat$OtherCMPN * dat$Deaths1) - dat$TB - dat$HIV
  }
  
  # Identify country/years where residual othercmpn deaths are lower than min threshold
  # Will need to squeeze the single causes for these country/years
  v_idSqz <- which(dat$OCDresid < dat$minCD)
  
  # Divide total othercmpn deaths by sum of (TB, HIV, Measles, minCD)
  # The latter quantities need to fit into othercmpn
  # The quotient is how much they must be scaled down to do so
  if (length(v_idSqz) > 0) {
    if("Measles" %in% names(dat)){
      v_scalingFactor <- (dat$OtherCMPN * dat$Deaths1)[v_idSqz] / (dat$TB + dat$HIV + dat$Measles + dat$minCD)[v_idSqz]
      dat$Measles[v_idSqz] <- dat$Measles[v_idSqz] * v_scalingFactor
    }else{
      v_scalingFactor <- (dat$OtherCMPN * dat$Deaths1)[v_idSqz] / (dat$TB + dat$HIV + dat$minCD)[v_idSqz]
    }
    # Scale deaths
    dat$TB[v_idSqz] <- dat$TB[v_idSqz] * v_scalingFactor
    dat$HIV[v_idSqz] <- dat$HIV[v_idSqz] * v_scalingFactor
    dat$OCDresid[v_idSqz] <- dat$minCD[v_idSqz] * v_scalingFactor
    # range(dat$TB + dat$HIV + dat$Measles + dat$OCDresid - dat$OtherCMPN * dat$Deaths1)
    # range(dat$TB[v_idSqz] + dat$HIV[v_idSqz] + dat$Measles[v_idSqz] + dat$OCDsq[v_idSqz] - (dat$OtherCMPN * dat$Deaths1)[v_idSqz])
  }
  
  # Convert to fractions
  # If there are zero crisis-free deaths, recode fraction as zero
  if("Measles" %in% names(dat)){
    dat$Measles <- dat$Measles / dat$Deaths1
    dat$Measles[is.na(dat$Measles)] <- 0
  }
  dat$TB <- dat$TB/dat$Deaths1
  dat$TB[is.na(dat$TB)] <- 0
  dat$HIV <- dat$HIV/dat$Deaths1
  dat$HIV[is.na(dat$HIV)] <- 0
  dat$OtherCMPN <- dat$OCDresid/dat$Deaths1
  dat$OtherCMPN[is.na(dat$OtherCMPN)] <- 0
  
  return(dat)
  
}
