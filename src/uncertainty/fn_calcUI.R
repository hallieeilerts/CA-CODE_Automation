fn_calcUI <- function(L_CSMFDRAWS, UI, CODALL, ENV = NULL, REGIONAL = FALSE){
  
  #' @title Calculate uncertainty intervals for fractions/rates/deaths from draws
  # 
  #' @description Create a separate list for fractions/rates/deaths.
  #' Convert each list of data frames to an array.
  #' Calculate the quantile for each cell across the matrices of the array.
  #' Save matrices with quantiles as data frames with identifying columns.
  #' Combine all data frames.
  #
  #' @param L_CSMFDRAWS List of length number of draws of predicted fractions.
  #' Each list element is a data frame with CSMFs for every country-year for all CODs estimated.
  #' Also contains columns c("ISO3", "Year", "Sex", "Deaths", "Rate")
  #' @param ENV
  #' @param UI Integer with the width of the uncertainty interval desired.
  #' @param KEY_COD Data frame with age-specific CODs with different levels of classification.
  #' @return Data frame with lower and upper quantiles for each COD for deaths, fractions, rates.
  
  # Create interval
  UI <- 1/2 + c(-UI, UI) / 2
  
  # Causes of death for this age group
  v_cod <- CODALL[CODALL %in% names(L_CSMFDRAWS[[1]])]
  # Causes of death for this age group
  #v_cod <- unique(KEY_COD$Reclass)  # Vector with ALL CAUSES OF DEATH (including single-cause estimates)
  #v_cod <- v_cod[!v_cod %in% c("Other", "Undetermined")]
  #v_cod <- v_cod[v_cod %in% names(L_CSMFDRAWS[[1]])]
  
  # One data frame with identifying columns that are shared across all draws
  df_idcols <- L_CSMFDRAWS[[1]][, !names(L_CSMFDRAWS[[1]]) %in% c("Deaths1", "Rate1", "Deaths2", "Rate2", paste(v_cod))]
  
  if(!is.null(ENV)){
    # All-cause rate from IGME envelope (for use in 2023.06.14 PATCH below)
    df_env <- merge(df_idcols, ENV[,c(idVars, "Rate2")], all.x = TRUE)
  }
  
  # Create lists for draws of fractions, rates, deaths
  l_frac   <- L_CSMFDRAWS
  l_rates  <- lapply(L_CSMFDRAWS, function(x){ x[,v_cod] <- x[,v_cod] * x[,"Rate2"] ; return(x)})
  l_deaths <- lapply(L_CSMFDRAWS, function(x){ x[,v_cod] <- x[,v_cod] * x[,"Deaths2"] ; return(x)})
  
  # Convert each data.frame in list to matrix that includes COD columns and all-cause deaths and rates
  l_frac   <- lapply(l_frac, function(x) as.matrix(x[,c("Deaths2", "Rate2", v_cod)]))
  l_deaths <- lapply(l_deaths, function(x) as.matrix(x[,c("Deaths2", "Rate2", v_cod)]))
  l_rates  <- lapply(l_rates, function(x) as.matrix(x[,c("Deaths2", "Rate2", v_cod)]))
  
  # Convert lists to arrays
  # Calculate quantiles for each cell across matrices of the array
  m_frac_lb <- apply(simplify2array(l_frac), c(1,2), quantile, UI[1], na.rm = T)
  m_frac_ub <- apply(simplify2array(l_frac), c(1,2), quantile, UI[2], na.rm = T)
  m_frac_med <- apply(simplify2array(l_frac), c(1,2), quantile, .5, na.rm = T)
  m_deaths_lb <- apply(simplify2array(l_deaths), c(1,2), quantile, UI[1], na.rm = T)
  m_deaths_ub <- apply(simplify2array(l_deaths), c(1,2), quantile, UI[2], na.rm = T)
  m_deaths_med <- apply(simplify2array(l_deaths), c(1,2), quantile, .5, na.rm = T)
  m_rates_lb <- apply(simplify2array(l_rates), c(1,2), quantile, UI[1], na.rm = T)
  m_rates_ub <- apply(simplify2array(l_rates), c(1,2), quantile, UI[2], na.rm = T)
  m_rates_med <- apply(simplify2array(l_rates), c(1,2), quantile, .5, na.rm = T)
  
  if(!is.null(ENV)){
    #------------------------#
    # 2023.06.14 PATCH
    # Adjust upper bound of all-cause mortality rate (and cause specific rates) if it is ever below IGME envelope point estimate.
    # From Pancho's function CalcUncert(). 
    # Check if the upper bound of the all-cause rate (column 2) is below IGME envelope.
    # If so, shift cause-specific and all-cause rate upper bound upwards for that country-year in all draws.
    # Recalculate quantiles for upper bounds.
    # Check again.
    idqx <- which(m_rates_ub[,2] < df_env$Rate2)
    idqx_check <- idqx
    if (length(idqx) > 0) {
      l_rates_Aux         <- lapply(l_rates, function(x) as.matrix(x[idqx, c("Deaths2","Rate2", v_cod)])  * 1.05 )
      m_rates_ub_Aux      <- apply(simplify2array(l_rates_Aux), c(1,2), quantile, .99, na.rm = T)
      m_rates_ub[idqx, ]  <- m_rates_ub_Aux
    }
    idqx <- which(m_rates_ub[,2] < df_env$Rate2)
    if (length(idqx) > 0) {
      l_rates_Aux         <- lapply(l_rates, function(x) as.matrix(x[idqx, c("Deaths2","Rate2", v_cod)])  * 1.1 )
      m_rates_ub_Aux      <- apply(simplify2array(l_rates_Aux), c(1,2), quantile, .99, na.rm = T)
      m_rates_ub[idqx, ]  <- m_rates_ub_Aux
    }
    idqx <- which(m_rates_ub[,2] < df_env$Rate2)
    if (length(idqx) > 0) {
      l_rates_Aux         <- lapply(l_rates, function(x) as.matrix(x[idqx, c("Deaths2","Rate2", v_cod)])  * 1.5 )
      m_rates_ub_Aux      <- apply(simplify2array(l_rates_Aux), c(1,2), quantile, .99, na.rm = T)
      m_rates_ub[idqx, ]  <- m_rates_ub_Aux
    }
    idqx <- which(m_rates_ub[,2] < df_env$Rate2)
    if (length(idqx) > 0) {
      l_rates_Aux         <- lapply(l_rates, function(x) as.matrix(x[idqx, c("Deaths2","Rate2", v_cod)])  * 2 )
      m_rates_ub_Aux      <- apply(simplify2array(l_rates_Aux), c(1,2), quantile, .99, na.rm = T)
      m_rates_ub[idqx, ]  <- m_rates_ub_Aux
    }
    idqx <- which(m_rates_ub[,2] < df_env$Rate2)
    if (length(idqx) > 0) {
      l_rates_Aux         <- lapply(l_rates, function(x) as.matrix(x[idqx, c("Deaths2","Rate2", v_cod)])  * 3 )
      m_rates_ub_Aux      <- apply(simplify2array(l_rates_Aux), c(1,2), quantile, .99, na.rm = T)
      m_rates_ub[idqx, ]  <- m_rates_ub_Aux
    }
    idqx <- which(m_rates_ub[,2] < df_env$Rate2)
    if (length(idqx) > 0) {
      l_rates_Aux         <- lapply(l_rates, function(x) as.matrix(x[idqx, c("Deaths2","Rate2", v_cod)])  * 4.5 )
      m_rates_ub_Aux      <- apply(simplify2array(l_rates_Aux), c(1,2), quantile, .999, na.rm = T)
      m_rates_ub[idqx, ]  <- m_rates_ub_Aux
    }
    # END PATCH
    #------------------------#
  }
  
  # Format arrays in to data frames
  df_frac_lb <- as.data.frame(cbind(df_idcols, 
                                    Variable = rep("Fraction", nrow(df_idcols)),
                                    Quantile = rep("Lower", nrow(df_idcols)), 
                                    m_frac_lb))
  df_frac_med <- as.data.frame(cbind(df_idcols, 
                                     Variable = rep("Fraction", nrow(df_idcols)),
                                     Quantile = rep("Median", nrow(df_idcols)), 
                                     m_frac_med))
  df_frac_ub <- as.data.frame(cbind(df_idcols, 
                                    Variable = rep("Fraction", nrow(df_idcols)),
                                    Quantile = rep("Upper", nrow(df_idcols)), 
                                    m_frac_ub))
  df_deaths_lb <- as.data.frame(cbind(df_idcols, 
                                      Variable = rep("Deaths", nrow(df_idcols)),
                                      Quantile = rep("Lower", nrow(df_idcols)), 
                                      m_deaths_lb))
  df_deaths_med <- as.data.frame(cbind(df_idcols, 
                                       Variable = rep("Deaths", nrow(df_idcols)),
                                       Quantile = rep("Median", nrow(df_idcols)), 
                                       m_deaths_med))
  df_deaths_ub <- as.data.frame(cbind(df_idcols, 
                                      Variable = rep("Deaths", nrow(df_idcols)),
                                      Quantile = rep("Upper", nrow(df_idcols)), 
                                      m_deaths_ub))  
  df_rates_lb <- as.data.frame(cbind(df_idcols, 
                                     Variable = rep("Rate", nrow(df_idcols)),
                                     Quantile = rep("Lower", nrow(df_idcols)), 
                                     m_rates_lb))
  df_rates_med <- as.data.frame(cbind(df_idcols, 
                                      Variable = rep("Rate", nrow(df_idcols)),
                                      Quantile = rep("Median", nrow(df_idcols)), 
                                      m_rates_med))
  df_rates_ub <- as.data.frame(cbind(df_idcols, 
                                     Variable = rep("Rate", nrow(df_idcols)),
                                     Quantile = rep("Upper", nrow(df_idcols)), 
                                     m_rates_ub))
  
  # Combine and tidy
  df_res <- rbind(df_frac_lb, df_frac_med, df_frac_ub, df_deaths_lb, df_deaths_ub, df_deaths_med, df_rates_lb, df_rates_ub, df_rates_med)
  if(!REGIONAL){
    df_res <- df_res[order(df_res$ISO3, df_res$Year, df_res$Sex, df_res$Variable, df_res$Quantile),]
  }else{
    df_res <- df_res[order(df_res$Region, df_res$Year, df_res$Sex, df_res$Variable, df_res$Quantile),]
  }
  rownames(df_res) <- NULL
  
  # Checking 2023.06.14 PATCH adjustment
  # idqx_check
  # 421  422 1387 1388 3763 4008 4009 4011 4012 4013 4014 4015
  #m_rates_ub[421,]
  #df_idcols[421,]
  #subset(df_env, ISO3 == "BIH" & Year == 2002)
  #subset(df_res, ISO3 == "BIH" & Year == 2002)
  
  return(df_res)
}
