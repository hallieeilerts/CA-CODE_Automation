fn_calcRegion <- function(CSMF, ENV_REGION = NULL, CODALL, KEY_REGION){
  
  #' @title Calculate CSMFs for global regions
  # 
  #' @description Converts CSMFs to deaths, back calculates denominator, aggregates deaths and population counts by region, recalculates CSMFs and all-cause mortality rate.
  #
  #' @param CSMF Data frame with CSMFs that have been processed by squeezing functions, CSMFs that were not squeezed (GOODVR), all-cause crisis-free and crisis-included deaths and rates.
  #' @param ENV_REGION
  #' @param CODALL
  #' @param KEY_REGION Data frame with countries and different regional classifications.
  #' @return Data frame with regional CSMFs, all-cause crisis-included deaths and rates.
  
  #fn_calcRegion(csmfSqz, env_REG, codAll, key_region)
  #CSMF <- csmfSqz
  #ENV_REGION <- env_REG
  #CODALL <- codAll
  #KEY_REGION <- key_region
  
  env_region <- ENV_REGION
  idVarsAux <- idVars
  idVarsAux[1] <- "Region"

  # Merge regions onto national estimates
  dat <- merge(CSMF, KEY_REGION, by = "ISO3")
  
  # Causes of death
  v_cod <- CODALL[CODALL %in% names(dat)]
  
  # Create unified variable for region
  dat$Region <- dat$UNICEFReportRegion1
  # If report region 2 is not missing, use it instead
  dat$Region[which(dat$UNICEFReportRegion2 != "")] <- dat$UNICEFReportRegion2[which(dat$UNICEFReportRegion2 != "")]
  
  # Manually add extra regions
  df_world <- dat
  df_world$Region <- "World"
  df_eca <- subset(dat, UNICEFReportRegion1 == "Europe and central Asia")
  df_eca$Region <- "Europe and central Asia"
  df_ssa <- subset(dat, UNICEFReportRegion1 == "Sub-Saharan Africa")
  df_ssa$Region <- "Sub-Saharan Africa"
  dat <- rbind(dat, df_world, df_eca, df_ssa)
  
  # Convert CSMFs to deaths
  dat[, paste(v_cod)] <- dat[, paste(v_cod)] * dat$Deaths2
  
  # Back calculate denominator from deaths and mortality rate
  dat$Px <- dat$Deaths2/dat$Rate2
  
  # Aggregate deaths and denominators for countries for each region
  dat <- ddply(dat, ~Region, function(x){aggregate(x[, c("Deaths2", "Px", paste(v_cod))], 
                                                   by = list(x$Year, x$Sex), sum, na.rm = T)})
  names(dat)[1:3] <- idVarsAux
  
  # Re-calculate CSMFs
  dat[, paste(v_cod)] <- dat[, paste(v_cod)] / dat$Deaths2
  
  # Re-calculate mortality rate
  dat$Rate2 <- dat$Deaths2 / dat$Px
  
  # Remove denominator
  dat <- dat[, names(dat) != "Px"]

  # Note (2023-09-30):
  # If regional envelopes have been provided by IGME, use them to replace deaths and rates for 10-14 and 15-19.
  # Do not do for 5-9, due to epidemic measles being added on top of envelope.
  if(ageLow %in% c(10, 15) & !is.null(ENV_REGION)){
    
    # Delete manually calculated all-cause deaths and rates columns
    dat <- dat[,c(idVarsAux, v_cod)]
    
    # Merge on IGME all-cause regional deaths and rates
    dat <- merge(dat, env_region, by = c('Region', 'Year'))
    
  }
  
  # Order columns
  dat <- dat[, c(idVarsAux, "Deaths2", "Rate2", v_cod)]
  
  # Tidy up
  dat <- dat[order(dat$Region, dat$Year, dat$Sex), ]
  dat$Region <- as.character(dat$Region)
  rownames(dat) <- NULL
  
  return(dat)
  
}