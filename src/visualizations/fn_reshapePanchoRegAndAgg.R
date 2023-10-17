fn_reshapePanchoRegAndAgg <- function(DAT, CODALL){
  
  #' @title Reshape Pancho's results for regions and aggregate age groups
  # 
  #' @description 
  #
  #' @param DAT 
  #' @return 
  
  dat <- DAT
  v_cod <- CODALL[CODALL %in% names(dat)]
  
  # Fix Sex if "T" was formatted as "TRUE" in csv file
  dat$Sex[dat$Sex == TRUE] <- sexLabels[1]
  
  # Only keep fractions
  # If fractions are not present, calculate from deaths
  if("Fraction" %in% unique(dat$Variable)){
    dat <- subset(dat, Variable == "Fraction")
  }else{
    dat[,v_cod] <- dat[,v_cod]/rowSums(dat[,v_cod])
    dat[is.na(dat)] <- 0
  }
  
  dat$Variable <- NULL
  dat$Quantile <- NULL
  
  if("Age" %in% names(dat)){
    dat$AgeLow[dat$Age == "10 to 19 years"] <- 10
    dat$AgeUp[dat$Age == "10 to 19 years"] <- 19
    dat$Age <- NULL
  }
  
  return(dat)
  
}
