fn_combineUIpoint <- function(UI, CSMFSQZ, CODALL, REGIONAL = FALSE){
  
  #' @title Combine raw data frames for CSMF point estimates and uncertainty intervals.
  # 
  #' @description Transforms CSMF point estimates into rates and deaths as well, combines with uncertainty intervals for CSMFs, rates, and deaths, orders rows and columns.
  #
  #' @param CSMFSQZ Data frame with CSMFs that have been processed in squeezing pipeline (contains all countries, even those not subject to squeezing).
  #' @param UI     Data frame with lower and upper quantiles for each COD for deaths, fractions, rates.
  #' @param KEY_COD Data frame with age-specific CODs with different levels of classification.
  #' @return Data frame with point estimates, lower, and upper bounds for CSMFs, deaths, and rates.
  
  # Causes of death for this age group
  v_cod <- CODALL[CODALL %in% names(CSMFSQZ)]
  #v_cod <- unique(KEY_COD$Reclass)  # Vector with ALL CAUSES OF DEATH (including single-cause estimates)
  #v_cod <- v_cod[!v_cod %in% c("Other", "Undetermined")]
  
  # Aux idVars
  if(!REGIONAL){
    idVarsAux <- idVars
  }else{
    idVarsAux <- c("Region", idVars[2:3])
  }
  
  # Only keep the following columns
  csmfSqz <- CSMFSQZ[,c(idVarsAux, "Deaths2", "Rate2", v_cod)]
  
  df_frac  <- csmfSqz
  df_rates <- csmfSqz
  df_deaths <- csmfSqz
  df_rates[,v_cod]  <- csmfSqz[,v_cod] * csmfSqz[,"Rate2"]
  df_deaths[,v_cod] <- csmfSqz[,v_cod] * csmfSqz[,"Deaths2"]
  
  # Add columns identifying point estimates
  df_frac$Variable   <- "Fraction"
  df_rates$Variable  <- "Rate"
  df_deaths$Variable <- "Deaths"
  df_frac$Quantile   <- "Point"
  df_rates$Quantile  <- "Point"
  df_deaths$Quantile <- "Point"
  
  # Keep same columns in UI
  v_col <- names(df_frac)[names(df_frac) %in% names(UI)]
  ui <- UI[,paste(v_col)]
  # Only keep upper and lower bounds (not median)
  ui <- subset(ui, Quantile %in% c("Lower", "Upper"))
  
  # Combine and tidy
  df_res <- rbind(df_frac, df_rates, df_deaths, ui)
  df_res <- df_res[, c(idVarsAux, "Deaths2", "Rate2", "Variable", "Quantile", v_cod)]
  if(!REGIONAL){
    df_res <- df_res[order(df_res$ISO3, df_res$Year, df_res$Sex, df_res$Variable, df_res$Quantile),]
  }else{
    df_res <- df_res[order(df_res$Region, df_res$Year, df_res$Sex, df_res$Variable, df_res$Quantile),]
  }
  rownames(df_res) <- NULL
  
  return(df_res)
  
}
