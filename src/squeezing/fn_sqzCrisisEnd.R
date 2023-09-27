fn_sqzCrisisEnd <- function(CSMF, KEY_COD, UNCERTAINTY = FALSE){
  
  #' @title Squeeze endemic crisis deaths
  # 
  #' @description Sum crisis-free deaths with endemic crisis single cause deaths.
  #' Calculate fractions for endemic crisis single cause deaths from this sum.
  #' 
  #' Subtract endemic crisis fractions from 1, squeeze other fractions into remaining space.
  #' @param CSMF Data frame with CSMFs that has been prepared for squeezing.
  #' @param KEY_COD Data frame with age-specific CODs with different levels of classification.
  #' @param UNCERTAINTY A boolean that denotes whether this function is being run as part of the squeezing pipeline or uncertainty pipeline.
  #' @return Data frame where CSMFs have been adjusted for endemic crisis squeezing.
  
  dat <- CSMF
  
  # Vector with all causes of death (including single-cause estimates)
  v_cod <- unique(KEY_COD$Reclass)  
  v_cod <- v_cod[!v_cod %in% c("Other", "Undetermined")]
  
  if(UNCERTAINTY == FALSE){
    # Add crisis-free deaths with endemic CollectVio and NatDis
    v_deaths <- dat$Deaths1 + dat$CollectVio + dat$NatDis
    
    # Calculate fraction of endemic collective violence (Pro-rata squeeze)
    dat$CollectVio <- dat$CollectVio/v_deaths
    
    # Calculate fraction of endemic natural disaster (Pro-rata squeeze)
    dat$NatDis <- dat$NatDis/v_deaths
    
    #' Note: This code is also included in the uncertainty function fn_rand_assign_crisisend(),
    #' so this if() statement avoids repeating it.
    #' In that function, a multinomial distribution is subsequently used to
    #' randomly sample counts for NatDis/CollectVio deaths with input parameters of the
    #' fractions just calculated and Deaths1 as Total count.
    #' The sampled counts are then divided by Deaths1 to get the final fractions.
  }
  
  # Squeeze other causes into remaining fraction
  dat[, paste(v_cod[which(!v_cod %in% c("CollectVio", "NatDis"))])] <- 
    dat[, paste(v_cod[which(!v_cod %in% c("CollectVio", "NatDis"))])] * (1 - dat$CollectVio - dat$NatDis)
  
  return(dat)
  
}
