fn_reshapePanchoRegional <- function(DAT){
  
  #' @title Plot point estimates and uncertainty intervals
  # 
  #' @description 
  #
  #' @param DAT 
  #' @return 
  
  dat <- DAT
  
  dat$Sex[dat$Sex == TRUE] <- sexLabels[1]
  
  dat <- subset(dat, Variable == "Fraction")
  
  dat$Variable <- NULL
  dat$Quantile <- NULL
  
  return(dat)
  
}
