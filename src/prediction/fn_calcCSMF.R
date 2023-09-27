fn_calcCSMF <- function(DAT, KEY_CTRYCLASS, KEY_COD, CTRYGRP){
  
  #' @title Calculate CSMFs for GoodVR and China
  # 
  #' @description Takes cleaned VR/China data and calculates CSMFs
  #
  #' @param DAT Data frame with formatted VR or China mortality data.
  #' @param KEY_CTRYCLASS Data frame which labels countries as HMM, LMM, or VR.
  #' @param KEY_COD Data frame with age-specific CODs with different levels of classification.
  #' @param CTRYGRP Character string that must be set as either "GOODVR" or "CHN".
  #' @return Data frame with CSMFs.
  
  if(!(CTRYGRP %in% c("GOODVR", "CHN"))){
    stop("Must set CTRYGRP as either GOODVR or CHN.")
  }
  
  dat <- DAT
  
  # Add missing categories to match VA COD list
  if (!"typhoid" %in% names(dat)) dat$typhoid <- 0
  if (!"other" %in% names(dat)) dat$other <- 0
  if (!"undt" %in% names(dat)) dat$undt <- 0
  if (!"hiv" %in% names(dat)) dat$hiv <- 0
  if (!"mal" %in% names(dat)) dat$mal <- 0
  
  # Select age of interest
  dat <- dat[dat$AgeLow == ageLow, ]
  # Select years of interest
  dat <- dat[dat$Year %in% Years, ]
  # Select countries of interest
  v_ctries <- c(KEY_CTRYCLASS$ISO3[KEY_CTRYCLASS$Group2010 == "VR"], "CHN")
  dat <- dat[dat$ISO3 %in% v_ctries, ]
  
  # Vector with all CODs (including single-cause estimates)
  v_cod <- unique(KEY_COD$Reclass)
  v_cod <- v_cod[!v_cod %in% c("Other", "Undetermined")]
  # If China, also exclude HIV as this will be added through squeezing
  if(CTRYGRP == "CHN"){ 
    v_cod <- v_cod[v_cod != "HIV"]
  }
  
  # Re-classify causes of death
  for(i in 1:length(v_cod)){
    orig <- KEY_COD$Original[KEY_COD$Reclass == v_cod[i]]
    if (length(orig) > 1) {
      dat[, paste(v_cod[i])] <- apply(dat[, paste(orig)], 1, 
                                      function(x) {
                                        if (all(is.na(x))) {
                                          return(NA)
                                        } else return(sum(x, na.rm = T))
                                      })
    } else dat[, paste(v_cod[i])] <- dat[, paste(orig)]
  }
  
  # Select idvars and COD columns
  dat <- dat[, names(dat) %in% c(idVars, v_cod)]
  
  # Delete unnecessary columns
  idExclude <- which(!KEY_COD$Original %in% v_cod)
  if (length(idExclude) > 0) {
    dat <- dat[, !names(dat) %in% paste(KEY_COD$Original[idExclude])]
  }
  
  # For GOODVR, collapse data points when there is no sex-split
  # These data points are already collapsed for China
  if(CTRYGRP == "GOODVR"){
    if(!sexSplit){
      dat <- aggregate(dat[, -which(names(dat) %in% idVars)], list(dat$ISO3, dat$Year), sum)
      names(dat)[names(dat) == "Group.1"] <- "ISO3"
      names(dat)[names(dat) == "Group.2"] <- "Year"
      dat$Sex <- sexLabels[1]
    }
  }
  
  # For GOODVR, convert deaths to fractions
  # For China, the CODs are already fractions
  if(CTRYGRP == "GOODVR"){
    dat[, !names(dat) %in% idVars] <- dat[, !names(dat) %in% idVars] / rowSums(dat[, !names(dat) %in% idVars])
  }
  
  # Adjust when 0 deaths
  idAdjust <- which(is.na(dat$OtherCMPN))
  if (length(idAdjust) > 0) {
    for (i in idAdjust) {
      if (dat$Year[i] == min(Years)) {
        dat[i, !names(dat) %in% idVars] <-
          dat[i+1, !names(dat) %in% idVars]  
      } else {
        dat[i, !names(dat) %in% idVars] <-
          dat[i-1, !names(dat) %in% idVars]
      }
    }
  }
  
  #----------------------#
  # PATCH 2022.09.28
  # Apply estimates from 2019 to 2020 and 2021
  dat2020 <- dat[dat$Year == 2019, ]
  dat2020$Year <- 2020
  dat <- rbind(dat, dat2020)
  dat2020$Year <- 2021
  dat <- rbind(dat, dat2020)
  rm(dat2020)
  # END PATCH
  #----------------------#
  
  # 15-19 years
  if (sexSplit) {
    dat <- subset(dat, Sex == sexLabel)
  }
  
  # Tidy up
  dat <- dat[order(dat$ISO3, dat$Year, dat$Sex), ]
  rownames(dat) <- NULL
  
  return(dat)
  
}
