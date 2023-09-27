fn_extractCov <- function(MOD_COVNAMES, DAT_PRED, KEY_CTRYCLASS, CTRYGRP){
  
  #' @title Extract covariate values from prediction database
  # 
  #' @description Extracts values from prediction database for covariates used in the age/sex-specific model. 
  #' Extracts sex-specific covariates for 15-19 age groups
  #' Smoothed covariate values for all.
  #
  #' @param MOD_COVNAMES Vector with names of covariates used in the age/sex-specific model. 
  #' @param DAT_PRED Data frame with prediction database.
  #' @param KEY_CTRYCLASS Data frame which labels countries as HMM, LMM, or VR.
  #' @param CTRYGRP Character string that must be set as either "HMM" or "LMM".
  #' @return Data frame with selected covariates from prediction database. 
  
  if(!(CTRYGRP %in% c("HMM", "LMM"))){
    stop("Must set CTRYGRP as either HMM or LMM.")
  }
  
  # Make another vector of covariates in model
  mod_covNamesAUX <- MOD_COVNAMES
  
  # Identify sex-specific covariates and replace
  mod_covNamesAUX[paste(mod_covNamesAUX, sexSuffix, sep = "_") %in% names(DAT_PRED)] <- paste(mod_covNamesAUX, sexSuffix, sep = "_")[paste(mod_covNamesAUX, sexSuffix, sep = "_") %in% names(DAT_PRED)]
  
  # Identify smoothed covariate values and replace
  mod_covNamesAUX[paste(mod_covNamesAUX, "sm", sep = "_") %in% names(DAT_PRED)] <- paste(mod_covNamesAUX, "sm", sep = "_")[paste(mod_covNamesAUX, "sm", sep = "_") %in% names(DAT_PRED)]
  
  # Select covariates from original database
  dat <- DAT_PRED[, names(DAT_PRED) %in% c("ISO3", "Year", mod_covNamesAUX)]
  
  # After selecting sex-specific and smoothed covariates, rename with original names of covariates in model object
  dat <- dat[, c("ISO3", "Year", mod_covNamesAUX)]
  names(dat)[-c(1:2)] <- MOD_COVNAMES
  
  # Select countries and update database
  dat <- dat[dat$ISO3 %in% KEY_CTRYCLASS$ISO3[KEY_CTRYCLASS$Group2010 == CTRYGRP], ]
  
  return(dat)
  
}
