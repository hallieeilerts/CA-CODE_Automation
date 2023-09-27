

fn_round_csmfsqz <- function(CSMFSQZ, KEY_COD){
  
  #' @title Round squeezed CSMFs
  # 
  #' @description Rounds all-cause deaths/rates and squeezed CSMFs. This is done in case we want to share our CSMF estimates prior to being ready to run the uncertainty pipeline. The uncertainty pipeline will round the point estimates (see fn_round_pointint()) and do minor adjustments some of the point estimates (see fn_adjust_pointint()). However the uncertainty pipeline may not be ready to be run due to missing inputs.
  #
  #' @param CSMFSQZ Data frame with CSMFs that have been processed in squeezing pipeline (contains all countries, even those not subject to squeezing).
  #' @param KEY_COD Data frame with age-specific CODs with different levels of classification.
  #' @return Data frame with all-cause deaths/rates and squeezed CSMFs rounded to the same number of digits as the function fn_round_point_int() in the uncertainty pipeline.
  
  dat <- data.frame(CSMFSQZ)
  
  # Causes of death for this age group
  v_cod <- unique(KEY_COD$Reclass)  # Vector with ALL CAUSES OF DEATH (including single-cause estimates)
  v_cod <- v_cod[!v_cod %in% c("Other", "Undetermined")]
  v_cod <- v_cod[v_cod %in% names(dat)]
  
  # Round all-cause deaths
  dat$Deaths2 <- round(dat$Deaths2)
  
  # Round all-cause rate
  dat$Rate2 <- round(dat$Rate2, 5)
  
  # Round cause-specific fractions
  dat[,v_cod] <- round(dat[,v_cod], 5)
  
  # Tidy up
  rownames(dat) <- NULL
  return(dat)
  
}


fn_publish_estimates <- function(DAT, KEY_REGION, KEY_CTRYCLASS, CODALL, UNCERTAINTY = FALSE, REGIONAL = FALSE){
  
  #' @title Create final spreadsheet for results sharing for national estimates
  # 
  #' @description Adds identifying columns and orders CODs
  #
  #' @param DAT Data frame with CSMFs that have been processed in squeezing pipeline or point estimates, lower, and upper bounds for fractions/deaths/rates that have been processed in uncertainty pipeline
  #' @param KEY_REGION Data frame with countries and different regional classifications.
  #' @param KEY_CTRYCLASS Data frame which labels countries as HMM, LMM, or VR.
  #' @param CODALL Vector with CODs for all age groups in correct order.
  #' @param UNCERTAINTY Boolean to denote whether to format uncertainty estimates.
  #' @return Data frame with all identifying columns and CSMFs or fractions/deaths/and rates for each COD in correct order.
  
  dat <- DAT
  v_cod <- CODALL[CODALL %in% names(dat)]
  
  # Add age group
  if(!("AgeLow" %in% names(dat))){
    dat$AgeLow <- ageLow # From session variables
    dat$AgeUp <- ageUp   # From session variables
  }
  
  # Rename
  names(dat)[names(dat) == "Deaths2"] <- "Deaths"
  names(dat)[names(dat) == "Rate2"] <- "Rate"
  
  if(!REGIONAL){
    # Merge on regions
    dat <- merge(dat, KEY_REGION, by = "ISO3")
    
    # Merge on country class
    dat <- merge(dat, KEY_CTRYCLASS[,c("ISO3", "Group2010", "FragileState")])
    names(dat)[names(dat) == "Group2010"] <- "Model"
  }

  if(!UNCERTAINTY){
    # If creating point estimates sheet from pointInt (which contains "Variable" column with fractions/deaths/rates), 
    # only keep point estimates for fractions and remove Variable and Quantile columns
    if("Variable" %in% names(dat)){
      dat <- subset(dat, Quantile %in% c("Point", "Median") & Variable == "Fraction")
      dat <- data.frame(dat)[, !names(dat) %in% c("Variable", "Quantile")]
    }
  }else{
    # If creating uncertainty sheet, remove all-cause deaths and rate columns
    dat <- data.frame(dat)[, !names(dat) %in% c("Deaths", "Rate")]
  }
  
  # Order columns
  v_col_order <- c("Region", "ISO3", "Year", "AgeLow", "AgeUp", "Sex", "Model", "FragileState",
                   "WHOname", "SDGregion", "UNICEFReportRegion1", "UNICEFReportRegion2",
                   "Variable", "Quantile", "Deaths", "Rate", v_cod)
  v_cols <- v_col_order[v_col_order %in% names(dat)]
  dat <- dat[, v_cols]
  
  # Tidy up
  rownames(dat) <- NULL
  return(dat)
  
}
