fn_adjustPointIntZeroDeaths <- function(POINTINT, CODALL, REGIONAL = FALSE){
  
  #' @title Adjust point estimates, lower, and upper bounds when cause-specific or all-cause deaths are close to zero
  # 
  #' @description When cause-specific deaths point estimate is between 0 and 1, change LB of fractions/rates/deaths to 0.
  #' When cause-specific deaths point estimate = 0 and UB of cause-specific rate < cause-specific rate point estimate, change point estimate of rate to 0.
  #' When cause-specific deaths point estimate = 0 and UB of cause-specific fraction < cause-specific fraction point estimate, change point estimate of fraction to 0.
  #' When IGME all-cause deaths = 0, change cause-specific fractions/deaths/rates LB and point estimates to 0, change cause-specific UB for fractions/deaths to 1, change cause-specific UB for rates to same as all-cause UB, change cause-specific UB for fractions/rates/deaths Measles, Malaria, Natural Disasters, Collective Violence to 0.
  #' Adjust LB and UB of cause-specific fractions that fall on wrong side of point estimate.
  #
  #' @param POINTINT Data frame with rounded point estimates, lower, and upper bounds for fractions/deaths/rates
  #' @param CODALL Vector with CODs for all age groups in correct order.
  #' @param REGIONAL Boolean with true/false value if regional estimates.
  #' @return Data frame with rounded point estimates, lower, and upper bounds for fractions/deaths/rates that have been adjusted for inconsistencies.
  
  dat <- POINTINT
  v_cod <- CODALL[CODALL %in% names(dat)]
  v_other <- names(dat)[!(names(dat) %in% v_cod)]
  
  ## Adjustments to cause-specific fractions/rates/deaths due to cause-specific deaths between 0 and 1
  
  # Reshape to long
  datLong <- melt(setDT(dat), measure.vars = v_cod, variable.name = "cod")
  datLong$cod <- as.character(datLong$cod)
  
  if(REGIONAL){
    datLong$id <- paste0(datLong$Region, datLong$Year, datLong$Sex, datLong$cod)
  }else{
    datLong$id <- paste0(datLong$ISO3, datLong$Year, datLong$Sex, datLong$cod)
  }
  
  # When cause-specific deaths point estimate <= 1, change lower bound of fractions/rates/deaths to 0
  id_lbAllADJ <- unique(subset(datLong, Variable == "Deaths" & Quantile == "Point" & value <= 1)$id)
  datLong$value[datLong$id %in% id_lbAllADJ & datLong$Quantile == "Lower"] <- 0
  
  # Reshape to wide to identify more values in need of adjustment
  # All adjustments will still be made to datLong
  datWide <- dcast(setDT(datLong),  id ~ Variable+Quantile, value.var = "value")
  
  # When cause-specific deaths point estimate is between 0 and 1 and also larger than the upper bound, change the upper bound to 1
  # Note: this one commented out  in Pancho's function AdjustUncert()
  #id_ubDeathsADJ <- unique(subset(datWide, Deaths_Point > 0 & Deaths_Point < 1 & Deaths_Point > Deaths_Upper)$id)
  #datLong$value[datLong$id %in% id_ubDeathsADJ & datLong$Quantile == "Upper" & datLong$Variable == "Deaths"] <- 1
  
  # When cause-specific deaths point estimate = 0 and upper bound of cause-specific rate < cause-specific rate point estimate, change point estimate of rate to 0.
  id_pointRateADJ <- unique(subset(datWide, Deaths_Point == 0 & Rate_Upper < Rate_Point)$id)
  datLong$value[datLong$id %in% id_pointRateADJ & datLong$Quantile == "Point" & datLong$Variable == "Rate"] <- 0
  
  # When cause-specific deaths point estimate = 0 and upper bound of cause-specific fraction < cause-specific fraction point estimate, change point estimate of fraction to 0.
  id_pointFracADJ <- unique(subset(datWide, Deaths_Point == 0 & Fraction_Upper < Fraction_Point)$id)
  datLong$value[datLong$id %in% id_pointFracADJ & datLong$Quantile == "Point" & datLong$Variable == "Fraction"] <- 0
  
  ## Adjustments to cause-specific fractions/rates/deaths due to zero IGME all-cause deaths
  
  if(!REGIONAL){
    
    # Country-years with 0 IGME deaths
    id_noIGMEDeaths <- unique(subset(datLong, Deaths2 == 0 & Quantile == "Point")$id)
    
    # Change lower bound and point estimate to 0.
    datLong$value[datLong$id %in% id_noIGMEDeaths & datLong$Quantile == "Lower"] <- 0
    datLong$value[datLong$id %in% id_noIGMEDeaths & datLong$Quantile == "Point"] <- 0
    
    # Change cause-specific upper bounds for fractions and deaths to 1.
    datLong$value[datLong$id %in% id_noIGMEDeaths & datLong$Quantile == "Upper"& datLong$Variable %in% c("Fraction", "Deaths")] <- 1
    
    # Change cause-specific upper bounds for rates to the same as the all-cause upper bound.
    datLong$value[datLong$id %in% id_noIGMEDeaths & datLong$Quantile == "Upper" & datLong$Variable == "Rate"] <- 
      datLong$Rate[datLong$id %in% id_noIGMEDeaths & datLong$Quantile == "Upper" & datLong$Variable == "Rate"]
    
    # Change certain cause-specific upper bounds for fractions/rates/deaths to 0.
    # Measles
    if("Measles" %in% v_cod){
      datLong$value[datLong$id %in% id_noIGMEDeaths & datLong$Quantile == "Upper" & datLong$cod == "Measles"] <- 0  
    }
    # Malaria
    if("Malaria" %in% v_cod){
      datLong$value[datLong$id %in% id_noIGMEDeaths & datLong$Quantile == "Upper" & datLong$cod == "Malaria"] <- 0  
    }
    # Natural disasters
    if("NatDis" %in% v_cod){
      datLong$value[datLong$id %in% id_noIGMEDeaths & datLong$Quantile == "Upper" & datLong$cod == "NatDis"] <- 0  
    }
    # Collective violence
    if("CollectVio" %in% v_cod){
      datLong$value[datLong$id %in% id_noIGMEDeaths & datLong$Quantile == "Upper" & datLong$cod == "CollectVio"] <- 0  
    }
  }
  
  # Reshape to wide to identify more values in need of adjustment
  # All adjustments will still be made to datLong
  datWide <- dcast(setDT(datLong),  id ~ Variable+Quantile, value.var = "value")
  
  ## Adjustments to lower/upper bounds of cause-specific fractions that fall on wrong side of point estimate
  id_lowerFracADJ <- unique(subset(datWide, Fraction_Lower > Fraction_Point)$id)
  datLong$value[datLong$id %in% id_lowerFracADJ & datLong$Quantile == "Lower" & datLong$Variable == "Fraction"] <- 
    datLong$value[datLong$id %in% id_lowerFracADJ & datLong$Quantile == "Lower" & datLong$Variable == "Deaths"]/
    datLong$Deaths2[datLong$id %in% id_lowerFracADJ & datLong$Quantile == "Point" & datLong$Variable == "Deaths"]
  
  id_upperFracADJ <- unique(subset(datWide, Fraction_Upper < Fraction_Point)$id)
  datLong$value[datLong$id %in% id_upperFracADJ & datLong$Quantile == "Upper" & datLong$Variable == "Fraction"] <- 
    datLong$value[datLong$id %in% id_upperFracADJ & datLong$Quantile == "Upper" & datLong$Variable == "Deaths"]/
    datLong$Deaths2[datLong$id %in% id_upperFracADJ & datLong$Quantile == "Point" & datLong$Variable == "Deaths"]
  
  # Reshape datLong to original form
  df_res <- datLong
  df_res$id <- NULL
  df_res <- dcast(df_res, ... ~ cod, value.var = "value")
  df_res <- data.frame(df_res)[, c(v_other, v_cod)]
  if(!REGIONAL){
    df_res <- df_res[order(df_res$ISO3, df_res$Year, df_res$Sex, df_res$Variable, df_res$Quantile),]
  }else{
    df_res <- df_res[order(df_res$Region, df_res$Year, df_res$Sex, df_res$Variable, df_res$Quantile),]
  }
  rownames(df_res) <- NULL
  
  return(df_res)
  
}
