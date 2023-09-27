fn_calcRegion <- function(CSMF, CODALL, KEY_REGION){
  
  #' @title Calculate CSMFs for global regions
  # 
  #' @description Converts CSMFs to deaths, back calculates denominator, aggregates deaths and population counts by region, recalculates CSMFs and all-cause mortality rate.
  #
  #' @param CSMF Data frame with CSMFs that have been processed by squeezing functions, CSMFs that were not squeezed (GOODVR), all-cause crisis-free and crisis-included deaths and rates.
  #' @param KEY_COD Data frame with age-specific CODs with different levels of classification.
  #' @param KEY_REGION Data frame with countries and different regional classifications.
  #' @return Data frame with regional CSMFs, all-cause crisis-included deaths and rates.
  
  # Merge on regions
  dat <- merge(CSMF, KEY_REGION, by = "ISO3")
  
  # Causes of death for this age group
  v_cod <- CODALL[CODALL %in% names(dat)]
  #v_cod <- unique(KEY_COD$Reclass)  # Vector with ALL CAUSES OF DEATH (including single-cause estimates)
  #v_cod <- v_cod[!v_cod %in% c("Other", "Undetermined")]
  
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
  
  # Create list of regions, move world to front
  v_regions <- sort(unique(dat$Region))
  v_regions <- v_regions[c(which(v_regions == "World"), which(v_regions != "World"))]
  
  # Convert CSMFs to deaths
  dat[, paste(v_cod)] <- dat[, paste(v_cod)] * dat$Deaths2
  
  # Back calculate denominator from deaths and mortality rate
  dat$Px <- dat$Deaths2/dat$Rate2
  
  # Aggregate countries for each region
  dat <- ddply(dat, ~Region, function(x){aggregate(x[, c("Deaths2", "Px", paste(v_cod))], 
                                                   by = list(x$Year, x$Sex), sum, na.rm = T)})
  names(dat)[1:3] <- c("Region", "Year", "Sex")
  
  # Re-calculate CSMFs
  dat[, paste(v_cod)] <- dat[, paste(v_cod)] / dat$Deaths2
  dat[, paste(v_cod)] <- dat[, paste(v_cod)] / rowSums(dat[, paste(v_cod)])
  
  # Re-calculate mortality rate
  dat$Rate2 <- dat$Deaths2 / dat$Px
  # Remove denominator
  dat <- dat[, names(dat) != "Px"]
  
  # Order columns
  dat <- dat[, c("Region", "Year","Sex", "Deaths2", "Rate2", v_cod)]
  
  # Tidy up
  dat$Region <- factor(dat$Region, levels = v_regions, ordered = TRUE)
  dat <- dat[order(dat$Region, dat$Year, dat$Sex), ]
  dat$Region <- as.character(dat$Region)
  rownames(dat) <- NULL
  
  return(dat)
  
}