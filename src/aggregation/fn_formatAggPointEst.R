fn_formatAggPointEst <- function(DAT, CODALL, REGIONAL = FALSE){
  
  #' @title Formats point estimates for aggregate age groups
  # 
  #' @description Convert fractions into cause-specific deaths and rates, add columns for variable and quantile, round cause-specific deaths, rates, fractions (see fn_roundPointInt())
  #' 
  #' @param DAT Data frame with CSMFs for aggregate age groups
  #' @param CODALL Vector with CODs for all age groups in correct order.
  #' @param REGIONAL boolean TRUE or FALSE
  #' @return Data frame with point estimates for cause-specific fractions, deaths, rates that can be combined with uncertainty
  
  #DAT <- csmfSqz_05to14
  #CODALL <- codAll
  
  # Causes of death for this age group
  v_cod <- CODALL[CODALL %in% names(DAT)]
  
  # One data frame with identifying columns that are shared across all draws
  df_idcols <- DAT[, !names(DAT) %in% c("Deaths1", "Deaths2", "Rate1", "Rate2", paste(v_cod))]
  
  # Create data frames of fractions, rates, deaths
  df_frac   <- DAT
  df_rates  <- DAT
  df_deaths <- DAT
  df_rates[,v_cod]  <- DAT[,v_cod] * DAT[,"Rate2"]
  df_deaths[,v_cod] <- DAT[,v_cod] * DAT[,"Deaths2"]
  
  # Add columns
  df_frac$Variable <- "Fraction"
  df_frac$Quantile <- "Point"
  df_rates$Variable <- "Rate"
  df_rates$Quantile <- "Point"
  df_deaths$Variable <- "Deaths"
  df_deaths$Quantile <- "Point"
  
  # Delete extra columns
  df_frac <-  df_frac[, !names(df_frac) %in% c("Deaths1", "Rate1")]
  df_rates <-  df_rates[, !names(df_rates) %in% c("Deaths1", "Rate1")]
  df_deaths <-  df_deaths[, !names(df_deaths) %in% c("Deaths1", "Rate1")]
  
  # Combine
  df_res <- rbind(df_frac, df_rates, df_deaths)
  
  # Tidy up
  df_res <- df_res[,c(names(df_idcols), "Variable", "Quantile", "Deaths2", "Rate2", v_cod)]
  if(!REGIONAL){
    df_res <- df_res[order(df_res$ISO3, df_res$Year, df_res$Sex, df_res$Variable, df_res$Quantile),]
  }else{
    df_res <- df_res[order(df_res$Region, df_res$Year, df_res$Sex, df_res$Variable, df_res$Quantile),]
  }
  rownames(df_res) <- NULL

  return(df_res)
}
