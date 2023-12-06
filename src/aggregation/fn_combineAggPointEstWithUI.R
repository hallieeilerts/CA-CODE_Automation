fn_combineAggPointEstWithUI <- function(DAT1, DAT2, REGIONAL = FALSE){
  
  #' @title Formats point estimates for aggregate age groups
  # 
  #' @description Convert fractions into cause-specific deaths and rates, add columns for variable and quantile
  #' 
  #' @param DAT1 Data frame with CSMFs for aggregate age groups that have been formatted to be able to be combined with uncertainty intervals
  #' @param DAT2 Data frame with uncertainty intervals for aggregate age groups
  #' @param CODALL Vector with CODs for all age groups in correct order.
  #' @param REGIONAL boolean TRUE or FALSE
  #' @return Data frame with point estimates for cause-specific fractions, deaths, rates that can be combined with uncertainty
  
  dat1 <- subset(DAT1, Quantile != "Median")
  dat2 <- subset(DAT2, Quantile != "Median")
  
  # Combine
  df_res <- rbind(dat1, dat2)
  
  # Tidy up
  if(!REGIONAL){
    df_res <- df_res[order(df_res$ISO3, df_res$Year, df_res$Sex, df_res$Variable, df_res$Quantile),]
  }else{
    df_res <- df_res[order(df_res$Region, df_res$Year, df_res$Sex, df_res$Variable, df_res$Quantile),]
  }
  rownames(df_res) <- NULL
  
  return(df_res)
}
