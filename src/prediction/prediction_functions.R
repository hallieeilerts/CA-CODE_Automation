
fn_calc_csmf <- function(DAT, KEY_CTRYCLASS, KEY_COD, CTRYGRP, ENV = NULL){
  
  #' @title Calculate CSMFs for GoodVR and China
  # 
  #' @description Takes cleaned VR/China data and calculates CSMFs
  #
  #' @param DAT Data frame with formatted VR or China mortality data.
  #' @param KEY_CTRYCLASS Data frame which labels countries as HMM, LMM, or VR.
  #' @param KEY_COD Data frame with age-specific CODs with different levels of classification.
  #' @param CTRYGRP Character string that must be set as either "GOODVR" or "CHN".
  #' @param ENV Data frame age-specific IGME envelopes for crisis-free and crisis-included deaths.
  #' @return Data frame with CSMFs.
  
  if(!(CTRYGRP %in% c("GOODVR", "CHN"))){
    stop("Must set CTRYGRP as either GOODVR or CHN.")
  }
  if(CTRYGRP == "GOODVR" & length(ENV) == 0){
    stop("Must provide argument for ENV.")
  }
  
  # Add missing categories to match VA COD list
  if (!"typhoid" %in% names(DAT)) DAT$typhoid <- 0
  if (!"other" %in% names(DAT)) DAT$other <- 0
  if (!"undt" %in% names(DAT)) DAT$undt <- 0
  if (!"hiv" %in% names(DAT)) DAT$hiv <- 0
  if (!"mal" %in% names(DAT)) DAT$mal <- 0
  
  # Select age of interest
  DAT <- DAT[DAT$AgeLow == ageLow, ]
  # Select years of interest
  DAT <- DAT[DAT$Year %in% Years, ]
  # Select countries of interest
  v_ctries <- c(KEY_CTRYCLASS$ISO3[KEY_CTRYCLASS$Group2010 == "VR"], "CHN")
  DAT <- DAT[DAT$ISO3 %in% v_ctries, ]
  
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
      DAT[, paste(v_cod[i])] <- apply(DAT[, paste(orig)], 1, 
                                      function(x) {
                                        if (all(is.na(x))) {
                                          return(NA)
                                        } else return(sum(x, na.rm = T))
                                      })
    } else DAT[, paste(v_cod[i])] <- DAT[, paste(orig)]
  }
  
  # Select idvars and COD columns
  DAT <- DAT[, names(DAT) %in% c(idVars, v_cod)]
  
  # Delete unnecessary columns
  idExclude <- which(!KEY_COD$Original %in% v_cod)
  if (length(idExclude) > 0) {
    DAT <- DAT[, !names(DAT) %in% paste(KEY_COD$Original[idExclude])]
  }
  
  # For GOODVR, collapse data points when there is no sex-split
  # These data points are already collapsed for China
  if(CTRYGRP == "GOODVR"){
    if(!sexSplit){
      DAT <- aggregate(DAT[, -which(names(DAT) %in% idVars)], list(DAT$ISO3, DAT$Year), sum)
      names(DAT)[names(DAT) == "Group.1"] <- "ISO3"
      names(DAT)[names(DAT) == "Group.2"] <- "Year"
      DAT$Sex <- sexLabels[1]
    }
  }
  
  # For GOODVR, convert deaths to fractions
  # For China, the CODs are already fractions
  if(CTRYGRP == "GOODVR"){
    DAT[, !names(DAT) %in% idVars] <- DAT[, !names(DAT) %in% idVars] / rowSums(DAT[, !names(DAT) %in% idVars])
  }
  
  # Adjust when 0 deaths
  idAdjust <- which(is.na(DAT$OtherCMPN))
  if (length(idAdjust) > 0) {
    for (i in idAdjust) {
      if (DAT$Year[i] == min(Years)) {
        DAT[i, !names(DAT) %in% idVars] <-
          DAT[i+1, !names(DAT) %in% idVars]  
      } else {
        DAT[i, !names(DAT) %in% idVars] <-
          DAT[i-1, !names(DAT) %in% idVars]
      }
    }
  }
  
  #----------------------#
  # PATCH 2022.09.28
  # Apply estimates from 2019 to 2020 and 2021
  dat2020 <- DAT[DAT$Year == 2019, ]
  dat2020$Year <- 2020
  DAT <- rbind(DAT, dat2020)
  dat2020$Year <- 2021
  DAT <- rbind(DAT, dat2020)
  rm(dat2020)
  # END PATCH
  #----------------------#
  
  # For GOODVR, merge on envelope
  # For China, envelopes will be merged on during squeezing
  if(CTRYGRP == "GOODVR"){
    ENV <- ENV[,c(idVars, "Deaths2","Rate2")]
    names(ENV)[names(ENV) == "Deaths2"] <- "Deaths"
    names(ENV)[names(ENV) == "Rate2"] <- "Rate"
    DAT <- merge(DAT, ENV, by = idVars, all.x = T)
  }
  
  # 15-19 years
  if (sexSplit) {
    DAT <- subset(DAT, Sex == sexLabel)
  }
  
  # Tidy up
  DAT <- DAT[order(DAT$ISO3, DAT$Year, DAT$Sex), ]
  rownames(DAT) <- NULL
  
  return(DAT)
  
}

fn_extract_cov <- function(VXF, DB_PRED, KEY_CTRYCLASS, CTRYGRP){
  
  #' @title Extract covariate values from prediction database
  # 
  #' @description Extracts values from prediction database for covariates used in the age/sex-specific model. 
  #' Extracts sex-specific covariates for 15-19 age groups
  #' Smoothed covariate values for all.
  #
  #' @param VXF Vector with names of covariates used in the age/sex-specific model. 
  #' @param DB_PRED Data frame with prediction database.
  #' @param KEY_CTRYCLASS Data frame which labels countries as HMM, LMM, or VR.
  #' @param CTRYGRP Character string that must be set as either "HMM" or "LMM".
  #' @return Data frame with selected covariates from prediction database. 
  
  if(!(CTRYGRP %in% c("HMM", "LMM"))){
    stop("Must set CTRYGRP as either HMM or LMM.")
  }
  
  # Make another vector of covariates in model
  vxfAux <- VXF
  
  # Identify sex-specific covariates and replace
  vxfAux[paste(vxfAux, sexSuffix, sep = "_") %in% names(DB_PRED)] <- paste(vxfAux, sexSuffix, sep = "_")[paste(vxfAux, sexSuffix, sep = "_") %in% names(DB_PRED)]
  
  # Identify smoothed covariate values and replace
  vxfAux[paste(vxfAux, "sm", sep = "_") %in% names(DB_PRED)] <- paste(vxfAux, "sm", sep = "_")[paste(vxfAux, "sm", sep = "_") %in% names(DB_PRED)]
  
  # Select covariates from original database
  DB_PRED <- DB_PRED[, names(DB_PRED) %in% c("ISO3", "Year", vxfAux)]
  
  # After selecting sex-specific and smoothed covariates, rename with names of covariates in model object
  DB_PRED <- DB_PRED[, c("ISO3", "Year", vxfAux)]
  names(DB_PRED)[-c(1:2)] <- VXF
  
  # Select countries and update database
  DB_PRED <- DB_PRED[DB_PRED$ISO3 %in% KEY_CTRYCLASS$ISO3[KEY_CTRYCLASS$Group2010 == CTRYGRP], ]
  
  return(DB_PRED)
  
}

fn_call_p1New <- function(PREDYEAR, FIT, DB_PRED, UNCERTAINTY = FALSE){
  
  #' @title Call function for fn_p1New() - prediction from one model estimation
  # 
  #' @description Runs fn_p1New and adds identifying columns
  #
  #' @param PREDYEAR Integer for year being predicted.
  #' @param FIT Model fit object.
  #' @param DB_PRED Data frame with selected covariates from prediction database for age/sex-specific model. 
  #' @param UNCERTAINTY Boolean for whether to return mean prediction or all draws from random effects.
  #' @return Data frame with selected covariates from prediction database. 

  # Subset to one year
  df_pred <- subset(DB_PRED, Year == PREDYEAR)
  
  # Call prediction function
  out <- fn_p1New(MO = FIT, PRED = df_pred, DEAT = NULL, PRAN = T, MEAN = !UNCERTAINTY)
  
  if(!UNCERTAINTY){
    # Format
    df_out <- as.data.frame(out$PF)
    df_out$ISO3 <- rownames(df_out)
    df_out$Year <- PREDYEAR
    rownames(df_out) <- NULL
    df_out$Sex <- sexLabel
    
    #----------------------#
    # PATCH 2023.05.15
    # Rename predicted CODs
    # Note: Use these names in model estimation next round so there's no need to adjust.
    names(df_out)[names(df_out) == "OtherCD"] <- "OtherCMPN"
    names(df_out)[names(df_out) == "RTA"] <- "RTI"
    names(df_out)[names(df_out) == "Self_harm"] <- "SelfHarm"
    names(df_out)[names(df_out) == "Interp_violence"] <- "InterpVio"
    names(df_out)[names(df_out) == "Other_inj"] <- "OtherInj"
    # END PATCH
    #----------------------#
    # Add empty "Maternal" column for 15-19 males
    if(sexLabel == sexLabels[3]){df_out$Maternal <- 0}
    
    # Return data frame
    return(df_out)
  }else{
    
    # Format
    l_out <- lapply(seq(dim(out$PF)[3]), function(x) out$PF[ , , x])
    l_out <- lapply(l_out, function(x){ x <- as.data.frame(x) ; return(x)})
    l_out <- lapply(l_out, function(x){ x$ISO3 <- rownames(x) ; return(x)})
    l_out <- lapply(l_out, function(x){ x$Year <- PREDYEAR    ; return(x)})
    l_out <- lapply(l_out, function(x){ rownames(x) <- NULL   ; return(x)})
    l_out <- lapply(l_out, function(x){ x$Sex <- sexLabel ; return(x)})
    
    #----------------------#
    # PATCH 2023.05.15
    # Rename predicted CODs
    # Note: Use these names in model estimation next round so there's no need to adjust.
    l_out <- lapply(l_out, function(x){ names(x)[names(x) == "OtherCD"] <- "OtherCMPN"
                                        names(x)[names(x) == "RTA"] <- "RTI"
                                        names(x)[names(x) == "Self_harm"] <- "SelfHarm"
                                        names(x)[names(x) == "Interp_violence"] <- "InterpVio"
                                        names(x)[names(x) == "Other_inj"] <- "OtherInj" ; return(x)})
    # END PATCH
    #----------------------#
    # Add empty "Maternal" column for 15-19 males
    if(sexLabel == sexLabels[3]){l_out <- lapply(l_out, function(x){ x$Maternal <- 0 ; return(x)})}
    
    # return list
    return(l_out)
    
  }

}

fn_p1New <- function(MO, PRED, DEAT, NDUM = 0, PFIX = T, PRAN = F, REME = 0, MEAN = T) {
  
  #' @title Prediction from one model estimation
  # 
  #' @description Predicts CSMFs for one year using model fit and covariate values
  #
  #' @param MO Model Output
  #' @param PRED Prediction database (Covariates)
  #' @param DEAT Deaths in countries to predict (IGME envelopes)
  #' @param NDUM Number of dummy variables
  #' @param PFIX Predict with fixed effects? (T/F)
  #' @param PRAN Predict with fixed+Random effects? (T/F)
  #' @param REME Put "0" in Re for studies without a Re in from estimation (otherwise use observed mean of Re)
  #' @param MEAN Calculate means based on parameter chains? (T/F) (TRUE for point estimates)
  #' @return List of length number of years being predicted.
  #' Each list element is a data frame with predicted CSMFs.
  
  # Number of data points
  S  <- nrow(PRED)
  # Number of variables in model
  K  <- nrow(MO$Vars)     
  # Vector of true causes of death
  TC <- MO$param$VDT
  
  # Burn-in period
  niter <- MO$output$n.iter
  burnin <- MO$output$BUGSoutput$n.burnin
  if (burnin < 2) {
    if (niter < 10000) {
      burnin <- max(1000, niter * .2)
    } else burnin <- niter * .4
  }
  
  # Thinning from JAGS output
  thin <- MO$output$BUGSoutput$n.thin
  
  # PREDICTION COVARIATES
  DX <- cbind(rep(1,S), as.matrix(PRED[, MO$Vars$xvar]))
  # MEAN and SD from study (estimation) dataset
  meanMat <- matrix(MO$Vars$mean, nrow = nrow(DX), ncol = K, byrow = T)
  sdMat <- matrix(MO$Vars$sd, nrow = nrow(DX), ncol = K, byrow = T)
  # NORMALISE
  if (NDUM > 0) {
    DX[, -c(1+0:NDUM)] <- (DX[, -c(1+0:NDUM)] - meanMat[, -c(1:NDUM)])/sdMat[, -c(1:NDUM)]
  } else DX[, -1] <- (DX[, -1] - meanMat)/sdMat
  rm(meanMat, sdMat)
  
  # LINEAR PREDICTOR after burn-in
  n <- dim(MO$output$BUGSoutput$sims.array)[1]
  # Extract only B coefficient estimates
  Bhat <- which(substr(dimnames(MO$output$BUGSoutput$sims.array)[[3]], 1, 2) == 'B[')
  # Estimated Beta coefficients
  Bhat <- MO$output$BUGSoutput$sims.array[(burnin/thin):n, , Bhat]
  
  # RANDOM EFFECTS
  if (PRAN) {
    
    # Recover RANDOM EFFECT from estimation
    rE <- which(substr(dimnames(MO$output$BUGSoutput$sims.array)[[3]], 1, 3) == 're[')
    rE <- MO$output$BUGSoutput$sims.array[(burnin/thin):n, , rE]
    
    # Add RANDOM EFFECT GROUP to PREDICTION dataset
    PRED <- merge(PRED, unique(MO$Studies[, c(2,3)]), by.x = names(PRED)[1],
                  by.y = names(MO$Studies)[2], all.x = T, all.y = F)
    
  }
  
  # For each iteration after BURN-IN
  for (i in 1:dim(Bhat)[1]) {
    
    # For each PARALLEL CHAIN
    for (j in 1:dim(Bhat)[2]) {
      
      # Extract beta coefficients
      B <- matrix(Bhat[i, j, ], nrow = K+1, ncol = length(TC)-1)
      
      # LINEAR PREDICTOR with FIXED EFFECTS
      LP <- DX %*% B 
      
      # RANDOM EFFECTS
      if (PRAN) {
        
        # Random effects matrix
        PR <- matrix(rE[i, j, ], ncol = length(TC)-1)
        # (Unweighted) Mean random effects
        MR <- colMeans(PR, na.rm = T)
        # Keep RE only from nationally representative studies in input database
        PR <- PR[PRED$rG, ]
        if (REME == 0) {
          # Assign 0 RE to countries NOT represented in input database
          PR[which(is.na(PR))] <- 0
        } else {
          # Assign MEAN RE to countries NOT represented in input database
          PR[which(is.na(PR))] <- MR[ceiling(which(is.na(PR))/nrow(PR))]
        }
        
        # Add RANDOM EFFECTS to LINEAR PREDICTOR
        LP <- LP + PR
        
      }
      
      # PREDICTED ODDS
      PF <- cbind(rep(1,S), exp(LP))
      
      # PREDICTED PROPORTIONS
      PF <- cbind(ISO3 = PRED$ISO3, as.data.frame(PF/rowSums(PF)))
      names(PF)[-1] <- paste0(TC)
      
      # Store results
      if (i == 1 & j == 1) {
        # Create new object to store results
        fracArray <- array(NA, dim = c(nrow(PF), ncol(PF) - 1, 
                                       dim(Bhat)[1] * dim(Bhat)[2]),
                           dimnames = list(PF[, 1], names(PF)[-1], NULL))
        fracArray[, , 1] <- as.matrix(PF[, -1])
      } else fracArray[, , j + (i - 1)*dim(Bhat)[2]] <- as.matrix(PF[, -1])
      
    }
  }
  
  # Point estimates (means)
  if (MEAN) {
    PF <- apply(fracArray, c(1, 2), mean)
  } else PF <- fracArray
  
  # Output
  return(list(Para_est = MO$param, Rand_eff = PRAN, PF = PF))
  
}

fn_cap_mal_frac <- function(DAT, DTH_MALARIA_5TO19, CSMF_MALARIA_01to04HMM){
  
  #' @title Cap malaria fractions
  # 
  #' @description Caps malaria fractions at country-year-specific fraction level for 1-4y
  #
  #' @param DAT Data frame with predicted CSMFs.
  #' @param DTH_MALARIA_5TO19 Data frame with columns c("ISO3", "Year", "dth_malaria_5to19")
  #' @param CSMF_MALARIA_01to04HMM Data frame with final CSMF results for 1-4y
  #' @return Data frame where predicted CSMFs have been adjusted for capped malaria fractions.
  
  # Identify COD
  v_cod <- names(DAT)
  v_cod <- v_cod[!(v_cod %in% idVars)]

  # Merge on malaria deaths
  DAT <- merge(DAT, DTH_MALARIA_5TO19, by = c("ISO3", "Year"), all.x = T)

  # Identify cases with 0 malaria
  idMal <- which(DAT$dth_malaria_5to19 == 0 | is.na(DAT$dth_malaria_5to19))

  # Force malaria to be 0
  if (length(idMal) > 0) {
    DAT[idMal, paste(v_cod)] <- DAT[idMal, paste(v_cod)]/rowSums(DAT[idMal, paste(v_cod[v_cod != "Malaria"])])
    DAT[idMal, "Malaria"] <- 0
  }
  
  # Remove unnecessary columns
  v_remove <- names(DTH_MALARIA_5TO19)[!(names(DTH_MALARIA_5TO19) %in% idVars)]
  DAT <- DAT[, !(names(DAT) %in% v_remove)]
  
  # Cap malaria to 1-59-month fractions
  DAT <- merge(DAT, CSMF_MALARIA_01to04HMM, by = c("ISO3", "Year"), all.x = T)
  
  # Assign 0 to NA (if any)
  DAT$csmf_malaria_01to04[which(is.na(DAT$csmf_malaria_01to04))] <- 0
  
  # Identify cases to update malaria
  idMal <- which(DAT$Malaria > DAT$csmf_malaria_01to04)
  
  # Cap malaria
  if (length(idMal) > 0) {
    DAT$Malaria[idMal] <- DAT$csmf_malaria_01to04[idMal]
    idCod <- v_cod[v_cod != "Malaria"]
    DAT[idMal, paste(idCod)] <- DAT[idMal, paste(idCod)] / rowSums(DAT[idMal, paste(paste(idCod))])
    DAT[idMal, paste(idCod)] <- DAT[idMal, paste(idCod)] * (1 - DAT$Malaria[idMal])
    rm(idCod)
  }
  
  # Remove unnecessary data
  v_remove <- names(CSMF_MALARIA_01to04HMM)[!(names(CSMF_MALARIA_01to04HMM) %in% idVars)]
  DAT <- DAT[, !(names(DAT) %in% v_remove)]
  
  # Tidy up
  DAT <- DAT[order(DAT$ISO3, DAT$Year),]
    
  return(DAT)
  
}

fn_set_mal_frac <- function(DAT){
  
  #' @title Set malaria fractions
  # 
  #' @description Adds a column for malaria fraction and sets to zero.
  #
  #' @param DAT Data frame with predicted CSMFs, of which malaria is not included.
  #' @return Dataframe with predicted CSMFs plus a new column for Malaria where all CSMFs are zero.
  
  DAT$Malaria <- 0

  return(DAT)
}

fn_format_prediction <- function(L_DAT_HMM, L_DAT_LMM){
  
  #' @title Format predicted CSMFs
  # 
  #' @description Combines predicted CSMFs for HMM and LMM countries.
  #
  #' @param L_DAT_HMM  List of length number of years being predicted. Each list element is a dataframe with predicted CSMFs for HMM.
  #' @param L_DAT_LMM List of length number of years being predicted. Each list element is a dataframe with predicted CSMFs for HMM.
  #' @return Dataframe with predicted CSMFs for HMM and LMM.
  
  datHMM <- do.call(rbind, L_DAT_HMM)
  datLMM <- do.call(rbind, L_DAT_LMM)
  dat <- rbind(datHMM, datLMM)
  
  # Rearrange columns
  dat <- dat[, c(idVars, sort(names(dat)[which(!names(dat) %in% idVars)]))] 
  
  # Tidy up
  dat <- dat[order(dat$ISO3, dat$Year),]
  
  return(dat)
  
}
