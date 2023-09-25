

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

  if(UNCERTAINTY == FALSE){
    # If creating point estimates sheet from pointInt (which contains "Variable" column with fractions/deaths/rates), 
    # only keep point estimates for fractions and remove Variable and Quantile columns
    if("Variable" %in% names(dat)){
      dat <- subset(dat, Quantile == "Point" & Variable == "Fraction")
      dat <- dat[, !names(dat) %in% c("Variable", "Quantile")]
    }
  }else{
    # If creating uncertainty sheet, remove all-cause deaths and rate columns
    dat <- dat[, !names(dat) %in% c("Deaths", "Rate")]
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


AGELB <- 5
AGEUB <- 14
CODALL <- codAll
ENV <- env_crisisincl_u20
CSMF05TO09 <- nat05to09
CSMF10TO14 <- nat10to14
  

fn_calc_agg_rate <- function(AGELB, AGEUB, CODALL, ENV, CSMF05TO09, CSMF10TO14, CSMF15TO19F, CSMF15TO19M){
  
  #' @title Calculate deaths, rates, CSMFs for non-standard age intervals
  # 
  #' @description Calculates point estimates for aggregate age groups.
  #
  #' @param AGELB Integer denoting lower bound of age group (possible values: 5, 10, 15)
  #' @param AGEUB Integer denoting upper bound of age group  (possible values: 9, 14, 19)
  #' @param CODALL Vector with CODs for all age groups in correct order.
  #' @param ENV   Data frame IGME envelopes for crisis-included deaths and rates for all ages.
  #' @param CSMF[age-group]  Data frame of age-specific CSMFs in final format for sharing (published).
  #' @return Data frame with all identifying columns and point estimates for aggregate age groups.
  
  
  v_keepcols <- c(idVars, CODALL, "Rate2", "Region")
  env <- ENV
  
  # Transform fractions into deaths
  if(AGELB == 5){
    CSMF05TO09[, CODALL[CODALL %in% names(CSMF05TO09)]] <- CSMF05TO09[, CODALL[CODALL %in% names(CSMF05TO09)]] * CSMF05TO09$Deaths2
    CSMF05TO09 <- CSMF05TO09[,names(CSMF05TO09) %in% v_keepcols]
    CSMF10TO14[, CODALL[CODALL %in% names(CSMF10TO14)]] <- CSMF10TO14[, CODALL[CODALL %in% names(CSMF10TO14)]] * CSMF10TO14$Deaths2
    CSMF10TO14 <- CSMF10TO14[,names(CSMF10TO14) %in% v_keepcols]
  }
  if(AGELB == 10){
    #CSMF10TO14 <- read.csv(paste("./gen/results/output/PointEstimates_National_10to14_", resDate, ".csv", sep =""))
    CSMF10TO14[, CODALL[CODALL %in% names(CSMF10TO14)]] <- CSMF10TO14[, CODALL[CODALL %in% names(CSMF10TO14)]] * CSMF10TO14$Deaths2
    CSMF10TO14 <- CSMF10TO14[,names(CSMF10TO14) %in% v_keepcols]
  }
  if(AGEUB == 19){
    #CSMF15TO19F <- read.csv(paste("./gen/results/output/PointEstimates_National_15to19f_", resDate, ".csv", sep =""))
    CSMF15TO19F[, CODALL[CODALL %in% names(CSMF15TO19F)]] <- CSMF15TO19F[, CODALL[CODALL %in% names(CSMF15TO19F)]] * CSMF15TO19F$Deaths2
    #CSMF15TO19M <- read.csv(paste("./gen/results/output/PointEstimates_National_15to19m_", resDate, ".csv", sep =""))
    CSMF15TO19M[, CODALL[CODALL %in% names(CSMF15TO19M)]] <- CSMF15TO19M[, CODALL[CODALL %in% names(CSMF15TO19M)]] * CSMF15TO19M$Deaths2
    # Combine sexes
    dat15to19 <- CSMF15TO19F
    dat15to19[, CODALL[CODALL %in% names(dat15to19)]] <- dat15to19[, CODALL[CODALL %in% names(dat15to19)]] + CSMF15TO19M[, CODALL[CODALL %in% names(CSMF15TO19M)]]
    dat15to19$Sex[dat15to19$Sex == sexLabels[2]] <- sexLabels[1]
    dat15to19 <- dat15to19[,names(dat15to19) %in% v_keepcols]
    # Get sex-combined rate from IGME crisis-included envelope
    env <- subset(env, Sex == sexLabels[1] & AGELB == 15 & AGEUB == 19)[,c("ISO3", "Year", "Rate2")]
    names(env)[names(env) == "Rate2"] <- "Rate"
  }
  
  # Merge rates for different age groups
  if(AGELB == 5 & AGEUB == 19){
    l_df <- list(CSMF05TO09[, c("ISO3","Year","Rate2")], CSMF10TO14[, c("ISO3","Year","Rate2")], env[, c("ISO3","Year","Rate2")])
  }
  if(AGELB == 5 & AGEUB == 14){
    l_df <- list(CSMF05TO09[, c("ISO3","Year","Rate2")], CSMF10TO14[, c("ISO3","Year","Rate2")])
  }
  if(AGELB == 10 & AGEUB == 19){
    l_df <- list(CSMF10TO14[, c("ISO3","Year","Rate")], env[, c("ISO3","Year","Rate")])
  }
  if(AGELB == 15 & AGEUB == 19){
    l_df <- list(env)
  }
  if(length(l_df)>1){ 
    df_rate <- Reduce(function(x, y) merge(x, y, by = c("ISO3", "Year"), all=TRUE), l_df)
  }else{
    df_rate <- l_df[[1]]
  }
  names(df_rate)[names(df_rate) == "Rate2.x"] <- "Rate_df1" 
  names(df_rate)[names(df_rate) == "Rate2.y"] <- "Rate_df2"
  names(df_rate)[names(df_rate) == "Rate2"] <- "Rate_df3"
  
  # Calculate aggregate deaths and rate
  if(AGEUB - AGELB == 4){names(df_rate)[names(df_rate) == "Rate_df3"] <- "Rate2"}
  if(AGEUB - AGELB == 9){df_rate$Rate2 <- 1000 - (1000 - df_rate$Rate_df1)*(1 - df_rate$Rate_df2 / 1000)}
  if(AGEUB - AGELB == 14){df_rate$Rate2 <- 1000 - (1000 - df_rate$Rate_df1)*(1 - df_rate$Rate_df2 / 1000)*(1 - df_rate$Rate_df3 / 1000)}
  df_rate <- df_rate[, c("ISO3", "Year", "Rate2")]
  
  # Remove old Rate columns, add missing COD, rbind
  if(AGELB == 5 & AGEUB == 19){
    CSMF05TO09 <- CSMF05TO09[, !names(CSMF05TO09) %in% c("Rate2")]
    CSMF10TO14 <- CSMF10TO14[, !names(CSMF10TO14) %in% c("Rate2")]
    dat15to19 <- dat15to19[, !names(dat15to19) %in% c("Rate2")]
    addCOD <- CODALL[which(!CODALL %in% names(CSMF05TO09))]
    CSMF05TO09[, paste(addCOD)] <- 0
    addCOD <- CODALL[which(!CODALL %in% names(CSMF10TO14))]
    CSMF10TO14[, paste(addCOD)] <- 0
    addCOD <- CODALL[which(!CODALL %in% names(dat15to19))]
    dat15to19[, paste(addCOD)] <- 0
    dat <- rbind(CSMF05TO09, CSMF10TO14, dat15to19)
  }
  if(AGELB == 5 & AGEUB == 14){
    CSMF05TO09 <- CSMF05TO09[, !names(CSMF05TO09) %in% c("Rate2")]
    CSMF10TO14 <- CSMF10TO14[, !names(CSMF10TO14) %in% c("Rate2")]
    addCOD <- names(CSMF10TO14)[!(names(CSMF10TO14) %in% names(CSMF05TO09))]
    CSMF05TO09[, paste(addCOD)] <- 0
    addCOD <- names(CSMF05TO09)[!(names(CSMF05TO09) %in% names(CSMF10TO14))]
    CSMF10TO14[, paste(addCOD)] <- 0
    dat <- rbind(CSMF05TO09, CSMF10TO14)
  }
  if(AGELB == 10 & AGEUB == 19){
    CSMF10TO14 <- CSMF10TO14[, !names(CSMF10TO14) %in% c("Rate2")]
    dat15to19 <- dat15to19[, !names(dat15to19) %in% c("Rate2")]
    addCOD <- names(dat15to19)[!(names(dat15to19) %in% names(CSMF10TO14))]
    CSMF10TO14[, paste(addCOD)] <- 0
    addCOD <- names(CSMF10TO14)[!(names(CSMF10TO14) %in% names(dat15to19))]
    dat15to19[, paste(addCOD)] <- 0
    dat <- rbind(CSMF10TO14, dat15to19)
  }
  if(AGELB == 15 & AGEUB == 19){
    dat15to19 <- dat15to19[, !names(dat15to19) %in% c("Rate")]
    dat <- dat15to19
  }
  
  # Aggregate ages
  dat <- aggregate(dat[, CODALL[CODALL %in% names(dat)]], by = list(dat$ISO3, dat$Year, dat$Sex), sum)
  names(dat)[1:3] <- idVars
  
  # Add rate2
  dat <- merge(dat, df_rate, by = c("ISO3", "Year"), all.x = T, all.y = F)
  
  # Back transform into fractions
  dat[, CODALL[CODALL %in% names(dat)]] <- round(dat[, CODALL[CODALL %in% names(dat)]] / rowSums(dat[, CODALL[CODALL %in% names(dat)]]), 5)
  
  # Tidy up
  dat <- dat[order(dat$ISO3, dat$Year), ]
  rownames(dat) <- NULL
  
  return(dat)
}
