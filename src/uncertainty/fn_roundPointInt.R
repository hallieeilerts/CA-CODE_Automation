fn_roundPointInt <- function(POINTINT, CODALL, REGIONAL = FALSE){
  
  #' @title Round point estimates, lower, and upper bounds for fractions/deaths/and rates
  # 
  #' @description Rounds estimates, lower, and upper bounds for fractions/deaths/and rates
  #' When cause-specific deaths point estimate is between 0 and 1, change LB of fractions/rates/deaths to 0.
  #
  #' @param POINTINT Data frame with point estimates, lower, and upper bounds for fractions/deaths/and rates
  #' @param KEY_COD Data frame with age-specific CODs with different levels of classification.
  #' @param REGIONAL Boolean with true/false value if regional estimates.
  #' @return Data frame with rounded point estimates, lower, and upper bounds for fractions/deaths/and rates
  
  dat <- data.frame(POINTINT)
  
  # Causes of death for this age group
  v_cod <- CODALL[CODALL %in% names(dat)]
  #v_cod <- unique(KEY_COD$Reclass)  # Vector with ALL CAUSES OF DEATH (including single-cause estimates)
  #v_cod <- v_cod[!v_cod %in% c("Other", "Undetermined")]
  
  ## Deaths
  
  # Round quantiles for all-cause deaths
  dat$Deaths2[dat$Quantile == "Median"] <- round(dat$Deaths2[dat$Quantile == "Median"])
  dat$Deaths2[dat$Quantile == "Point"] <- round(dat$Deaths2[dat$Quantile == "Point"])
  dat$Deaths2[dat$Quantile == "Lower"] <- floor(dat$Deaths2[dat$Quantile == "Lower"])
  dat$Deaths2[dat$Quantile == "Upper"] <- ceiling(dat$Deaths2[dat$Quantile == "Upper"])
  
  # Round quantiles for cause-specific deaths
  dat[dat$Variable == "Deaths" & dat$Quantile == "Median", v_cod] <- 
    round(dat[dat$Variable == "Deaths" & dat$Quantile == "Median", v_cod])
  dat[dat$Variable == "Deaths" & dat$Quantile == "Point", v_cod] <- 
    round(dat[dat$Variable == "Deaths" & dat$Quantile == "Point", v_cod])
  dat[dat$Variable == "Deaths" & dat$Quantile == "Lower", v_cod] <- 
    floor(dat[dat$Variable == "Deaths" & dat$Quantile == "Lower", v_cod])
  dat[dat$Variable == "Deaths" & dat$Quantile == "Upper", v_cod] <- 
    ceiling(dat[dat$Variable == "Deaths" & dat$Quantile == "Upper", v_cod])
  
  ## Rates
  
  # Round all-cause rates
  dat$Rate2[dat$Quantile == "Median"] <- round(dat$Rate2[dat$Quantile == "Median"], 5)
  dat$Rate2[dat$Quantile == "Point"] <- round(dat$Rate2[dat$Quantile == "Point"], 5)
  dat$Rate2[dat$Quantile == "Lower"] <- floor(dat$Rate2[dat$Quantile == "Lower"]*10^5) / 10^5
  dat$Rate2[dat$Quantile == "Upper"] <- ceiling(dat$Rate2[dat$Quantile == "Upper"]*10^5) / 10^5
  
  # Round cause-specific rates
  dat[dat$Variable == "Rate" & dat$Quantile == "Point", v_cod] <- 
    round(dat[dat$Variable == "Rate" & dat$Quantile == "Median", v_cod], 5)
  dat[dat$Variable == "Rate" & dat$Quantile == "Point", v_cod] <- 
    round(dat[dat$Variable == "Rate" & dat$Quantile == "Point", v_cod], 5)
  dat[dat$Variable == "Rate" & dat$Quantile == "Lower", v_cod] <- 
    floor(dat[dat$Variable == "Rate" & dat$Quantile == "Lower", v_cod]*10^5) / 10^5
  dat[dat$Variable == "Rate" & dat$Quantile == "Upper", v_cod] <- 
    ceiling(dat[dat$Variable == "Rate" & dat$Quantile == "Upper", v_cod]*10^5) / 10^5
  
  ## Fractions
  
  # Round cause-specific fractions
  dat[dat$Variable == "Fraction" & dat$Quantile == "Median", v_cod] <- 
    round(dat[dat$Variable == "Fraction" & dat$Quantile == "Median", v_cod], 5)
  dat[dat$Variable == "Fraction" & dat$Quantile == "Point", v_cod] <- 
    round(dat[dat$Variable == "Fraction" & dat$Quantile == "Point", v_cod], 5)
  dat[dat$Variable == "Fraction" & dat$Quantile == "Lower", v_cod] <- 
    floor(dat[dat$Variable == "Fraction" & dat$Quantile == "Lower", v_cod]*10^5) / 10^5
  dat[dat$Variable == "Fraction" & dat$Quantile == "Upper", v_cod] <- 
    ceiling(dat[dat$Variable == "Fraction" & dat$Quantile == "Upper", v_cod]*10^5) / 10^5
  
  # Tidy
  if(!REGIONAL){
    dat <- dat[order(dat$ISO3, dat$Year, dat$Sex, dat$Variable, dat$Quantile),]
  }else{
    dat <- dat[order(dat$Region, dat$Year, dat$Sex, dat$Variable, dat$Quantile),]
  }
  rownames(dat) <- NULL
  
  return(dat)
  
}
