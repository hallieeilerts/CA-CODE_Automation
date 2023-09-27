fn_checkUI <- function(POINTINT, CODALL, REGIONAL = FALSE, QUANTILE = "point"){
  
  #' @title Check if point estimates fall inside uncertainty intervals
  # 
  #' @description Checks for country-years where point estimate for deaths, fractions, or rates are outside of uncertainty intervals.
  #
  #' @param POINTINT Data frame with rounded point estimates, lower, and upper bounds for fractions/deaths/rates
  #' @param KEY_COD Data frame with age-specific CODs with different levels of classification.
  #' @param QUANTILE String of either "point" or "median" to determine which should be checked
  #' @return Data frame with rows where death/fraction/rate point estimates are outside of uncertainty intervals.
  
  dat <- POINTINT
  
  # Causes of death for this age group
  v_cod <- CODALL[CODALL %in% names(dat)]
  #v_cod <- unique(KEY_COD$Reclass)  # Vector with ALL CAUSES OF DEATH (including single-cause estimates)
  #v_cod <- v_cod[!v_cod %in% c("Other", "Undetermined")]
  
  # Aux idVars
  if(!REGIONAL){
    idVarsAux <- idVars
  }else{
    idVarsAux <- c("Region", idVars[2:3])
  }
  
  # Reshape to long
  datLong <- melt(setDT(dat), measure.vars = v_cod, variable.name = "cod")
  
  # Reshape Variable and Quantile wide
  if(!REGIONAL){
    datWide <- dcast(datLong,  ISO3+Year+Sex+cod ~ Variable+Quantile, value.var = "value")
  }else{
    datWide <- dcast(datLong,  Region+Year+Sex+cod ~ Variable+Quantile, value.var = "value")
  }
  
  if(QUANTILE == "point"){
    # Check that point estimate is inside uncertainty interval
    datWide$Deaths_Check   <- ifelse(datWide$Deaths_Lower > datWide$Deaths_Point | datWide$Deaths_Upper < datWide$Deaths_Point, 1, 0)
    datWide$Fraction_Check <- ifelse(datWide$Fraction_Lower > datWide$Fraction_Point | datWide$Fraction_Upper < datWide$Fraction_Point, 1, 0)
    datWide$Rate_Check     <- ifelse(datWide$Rate_Lower > datWide$Rate_Point | datWide$Rate_Upper < datWide$Rate_Point, 1, 0)
  }
  if(QUANTILE == "median"){
    # Check that median is inside uncertainty interval
    datWide$Deaths_Check   <- ifelse(datWide$Deaths_Lower > datWide$Deaths_Median | datWide$Deaths_Upper < datWide$Deaths_Median, 1, 0)
    datWide$Fraction_Check <- ifelse(datWide$Fraction_Lower > datWide$Fraction_Median | datWide$Fraction_Upper < datWide$Fraction_Median, 1, 0)
    datWide$Rate_Check     <- ifelse(datWide$Rate_Lower > datWide$Rate_Median | datWide$Rate_Upper < datWide$Rate_Median, 1, 0)
  }
  
  # Reshape to long
  datLong2 <- melt(datWide, id.vars = c(idVarsAux, "cod", "Deaths_Check", "Fraction_Check", "Rate_Check"))
  
  # Keep rows where point estimate is outside of bounds
  df_prob <- subset(datLong2, Deaths_Check == 1 | Fraction_Check == 1 | Rate_Check == 1)
  df_prob$Variable <- sub('\\_.*', '', df_prob$variable)
  df_prob$Quantile <-sub('.*\\_', '', df_prob$variable)
  df_prob$flag <- 0
  df_prob$flag[df_prob$Deaths_Check == 1 & df_prob$Variable == "Deaths"] <- 1
  df_prob$flag[df_prob$Fraction_Check == 1 & df_prob$Variable == "Fraction"] <- 1
  df_prob$flag[df_prob$Rate_Check == 1 & df_prob$Variable == "Rate"] <- 1
  df_prob <- data.frame(df_prob)[,c(idVarsAux, "cod", "Variable", "Quantile", "value", "flag")]
  if(!REGIONAL){
    df_prob <- df_prob[order(df_prob$ISO3, df_prob$Year, df_prob$cod, df_prob$Variable, df_prob$Quantile),]
  }else{
    df_prob <- df_prob[order(df_prob$Region, df_prob$Year, df_prob$cod, df_prob$Variable, df_prob$Quantile),]
  }
  
  if(nrow(df_prob) >= 1){
    warning("Point estimates are outside bounds")
  }
  
  return(df_prob)
}
