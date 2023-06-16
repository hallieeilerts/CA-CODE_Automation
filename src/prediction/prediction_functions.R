
##################################################
####
####   Calculate CSMFs, GoodVR and China
####
##################################################

# group = country group; either GOODVR or CHN
fn_calc_csmf <- function(dat, key_ctryclass, key_cod, ctryGrp, env = NULL){
  
  if(!(ctryGrp %in% c("GOODVR", "CHN"))){
    stop("Must set ctryGrp as either GOODVR or CHN")
  }
  if(ctryGrp == "GOODVR" & length(env) == 0){
    stop("Must provide argument for env")
  }
  
  v_cod <- unique(key_cod$Reclass) # Vector with all CODs (including single-cause estimates)
  
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
  v_ctries <- c(key_ctryclass$ISO3[key_ctryclass$Group2010 == "VR"], "CHN")
  dat <- dat[dat$ISO3 %in% v_ctries, ]
  
  # Re-classify causes of death
  for(i in 1:length(v_cod)){
    orig <- key_cod$Original[key_cod$Reclass == v_cod[i]]
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
  idExclude <- which(!key_cod$Original %in% v_cod)
  if (length(idExclude) > 0) {
    dat <- dat[, !names(dat) %in% paste(key_cod$Original[idExclude])]
  }
  dat <- dat[, !names(dat) %in% c("Other", "Undetermined")]
  # If China, also delete HIV as this will be added through squeezing
  if(ctryGrp == "CHN"){
    dat <- dat[, !names(dat) %in% c("HIV")]
  }
  
  # For GOODVR, collapse data points when there is no sex-split
  # These data points are already collapsed for China
  if(ctryGrp == "GOODVR"){
    if(!sexSplit){
      dat <- aggregate(dat[, -which(names(dat) %in% idVars)], list(dat$ISO3, dat$Year), sum)
      names(dat)[names(dat) == "Group.1"] <- "ISO3"
      names(dat)[names(dat) == "Group.2"] <- "Year"
      dat$Sex <- sexLabels[1]
    }
  }
  
  # For GOODVR, convert deaths to fractions
  # For China, the CODs are already fractions
  if(ctryGrp == "GOODVR"){
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
  # BEGIN NEW 2022.09.28 #
  
  # APPLY ESTIMATES FROM 2019 TO 2020 AND 2021
  dat2020 <- dat[dat$Year == 2019, ]
  dat2020$Year <- 2020
  dat <- rbind(dat, dat2020)
  dat2020$Year <- 2021
  dat <- rbind(dat, dat2020)
  rm(dat2020)
  
  # END NEW              #
  #----------------------#
  
  # For GOODVR, merge on envelope
  # For China, envelopes will be merged on during squeezing
  if(ctryGrp == "GOODVR"){
    env <- env[,c(idVars, "Deaths2","Rate2")]
    names(env)[names(env) == "Deaths2"] <- "Deaths"
    names(env)[names(env) == "Rate2"] <- "Rate"
    dat <- merge(dat, env, by = idVars, all.x = T)
  }
  
  # 15-19 years
  if (sexSplit) {
    dat <- subset(dat, Sex == sexLabel)
  }
  
  # Tidy up
  dat <- dat[order(dat$ISO3, dat$Year, dat$Sex), ]
  rownames(dat) <- NULL
  
  return(dat)
  # Save output
  #write.csv(dat, paste("./gen/prediction/output/csmf_", ageGroup, group,".csv", sep=""), row.names = FALSE)
  
}

##################################################
####
####   Extract covariate values from prediction database
####
##################################################

fn_extract_cov <- function(vxf, db_pred, key_ctryclass, ctryGrp){
  
  # Make another vector of covariates in model
  vxfAux <- vxf
  
  # Identify sex-specific covariates and replace
  vxfAux[paste(vxfAux, sexSuffix, sep = "_") %in% names(db_pred)] <- paste(vxfAux, sexSuffix, sep = "_")[paste(vxfAux, sexSuffix, sep = "_") %in% names(db_pred)]
  
  # Identify smoothed covariate values and replace
  vxfAux[paste(vxfAux, "sm", sep = "_") %in% names(db_pred)] <- paste(vxfAux, "sm", sep = "_")[paste(vxfAux, "sm", sep = "_") %in% names(db_pred)]
  
  # Select covariates from original database
  db_pred <- db_pred[, names(db_pred) %in% c("ISO3", "Year", vxfAux)]
  
  # After selecting sex-specific and smoothed covariates, rename with names of covariates in model object
  db_pred <- db_pred[, c("ISO3", "Year", vxfAux)]
  names(db_pred)[-c(1:2)] <- vxf
  
  # Select countries and update database
  db_pred <- db_pred[db_pred$ISO3 %in% key_ctryclass$ISO3[key_ctryclass$Group2010 == ctryGrp], ]
  
  return(db_pred)
  #write.csv(db_pred, paste("./gen/prediction/input/mod_covVal_", ageGroup, ctryGrp, ".csv",sep=""), row.names = FALSE)
}



##################################################
####
####   Call function for prediction from ONE model estimation
####
##################################################

# Runs fn_p1New and adds identifying columns
# fn_call_p1New <- function(predyear, fit, covVal){
#   # Subset to one year
#   df_pred <- subset(covVal, Year == predyear)
#   # Call prediction function
#   out <- fn_p1New(MO = fit, PRED = df_pred, DEAT = NULL, PRAN = T, MEAN = T)
#   # Format as data frame
#   df_out <- as.data.frame(out$PF)
#   # Add identifying columns
#   df_out$ISO3 <- rownames(df_out)
#   rownames(df_out) <- NULL
#   df_out$Year <- predyear
#   return(df_out)
# }


#PREDYEAR <- 2005
#FIT <- fit_HMM
#DB_PRED <- db_pred_HMM
#UNCERTAINTY = TRUE

# Runs fn_p1New and adds identifying columns
fn_call_p1New <- function(PREDYEAR, FIT, DB_PRED, UNCERTAINTY = FALSE){
  
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
    
    # Rename predicted CODs
    # Note: Use these names in model estimation next round so there's no need to adjust.
    names(df_out)[names(df_out) == "OtherCD"] <- "OtherCMPN"
    names(df_out)[names(df_out) == "RTA"] <- "RTI"
    names(df_out)[names(df_out) == "Self_harm"] <- "SelfHarm"
    names(df_out)[names(df_out) == "Interp_violence"] <- "InterpVio"
    names(df_out)[names(df_out) == "Other_inj"] <- "OtherInj"
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
    
    # Rename predicted CODs
    # !!! Note: Use these names in model estimation next round so there's no need to adjust.
    l_out <- lapply(l_out, function(x){ names(x)[names(x) == "OtherCD"] <- "OtherCMPN"
                                        names(x)[names(x) == "RTA"] <- "RTI"
                                        names(x)[names(x) == "Self_harm"] <- "SelfHarm"
                                        names(x)[names(x) == "Interp_violence"] <- "InterpVio"
                                        
                                        names(x)[names(x) == "Other_inj"] <- "OtherInj" ; return(x)})
    # Add empty "Maternal" column for 15-19 males
    if(sexLabel == sexLabels[3]){l_out <- lapply(l_out, function(x){ x$Maternal <- 0 ; return(x)})}
    
    # return list
    return(l_out)
    
  }

}



##################################################
####
####   Prediction from ONE model estimation
####
##################################################

# Predicts CSMFs for one year using model fit, subset covariate values
fn_p1New <- function(MO, PRED, DEAT, NDUM = 0, PFIX = T, PRAN = F, REME = 0, MEAN = T) {
  
  ##  MO      Model Output
  ##  PRED    Prediction database (Covariates)
  ##  DEAT    Deaths in countries to predict (IGME envelopes)
  ##  NDUM    Number of dummy variables
  ##  PFIX    Predict with fixed effects? (T/F)
  ##  PRAN    Predict with fixed+Random effects? (T/F)
  ##  REME    Put "0" in Re for studies without a Re in from estimation (otherwise use observed mean of Re)
  ##  MEAN    Calculate means based on parameter chains? (T/F) (TRUE for point estimates)
  
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


##################################################
####
####   Cap malaria fractions at level of 1-4y
####
##################################################

#DAT <- l_draws_HMM[[1]]


fn_cap_mal_frac <- function(DAT, DTH_MALARIA_5TO19, CSMF_MALARIA_01to04HMM){
  
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


##################################################
####
####   Set malaria fraction to zero
####
##################################################

fn_set_mal_frac <- function(DAT){
  
  DAT$Malaria <- 0

  return(DAT)
}

# fn_set_mal_frac <- function(DAT, UNCERTAINTY = FALSE){
#   
#   if(!UNCERTAINTY){
#     DAT$Malaria <- 0
#   }else{
#     DAT <- lapply(DAT, function(x){ x$Malaria <- 0 ; return(x)})
#   }
#   return(DAT)
#   
# }




# fn_cap_mal_frac <- function(dat, key_ctryclass, dth_malaria_5to19, csmf_malaria_01to04HMM){
#   
#   # Identify HMM countries
#   v_ctries <- key_ctryclass$ISO3[key_ctryclass$Group2010 == "HMM"]
#   dat <- subset(dat, ISO3 %in% v_ctries)
#   datLMM <- subset(dat, !(ISO3 %in% v_ctries))
#   
#   # Identify COD
#   v_cod <- names(dat)
#   v_cod <- v_cod[!(v_cod %in% idVars)]
#   
#   # Merge on malaria deaths
#   dat <- merge(dat, dth_malaria_5to19, by = c("ISO3", "Year"), all.x = T)
#   
#   # Identify cases with 0 malaria
#   idMal <- which(dat$dth_malaria_5to19 == 0 | is.na(dat$dth_malaria_5to19))
#   
#   # Force malaria to be 0
#   if (length(idMal) > 0) {
#     dat[idMal, paste(v_cod)] <- dat[idMal, paste(v_cod)]/rowSums(dat[idMal, paste(v_cod[v_cod != "Malaria"])])
#     dat[idMal, "Malaria"] <- 0
#   }
#   
#   # Remove unnecessary data
#   v_remove <- names(dth_malaria_5to19)[!(names(dth_malaria_5to19) %in% idVars)]
#   dat <- dat[, !(names(dat) %in% v_remove)]
#   
#   # Cap malaria to 1-59-month fractions
#   dat <- merge(dat, csmf_malaria_01to04HMM, by = c("ISO3", "Year"), all.x = T)
#   
#   # Assign 0 to NA (if any)
#   dat$csmf_malaria_01to04[which(is.na(dat$csmf_malaria_01to04))] <- 0
#   
#   # Identify cases to update malaria
#   idMal <- which(dat$Malaria > dat$csmf_malaria_01to04)
#   
#   # Cap malaria
#   if (length(idMal) > 0) {
#     dat$Malaria[idMal] <- dat$csmf_malaria_01to04[idMal]
#     idCod <- v_cod[v_cod != "Malaria"]
#     dat[idMal, paste(idCod)] <- dat[idMal, paste(idCod)] / rowSums(dat[idMal, paste(paste(idCod))])
#     dat[idMal, paste(idCod)] <- dat[idMal, paste(idCod)] * (1 - dat$Malaria[idMal])
#     rm(idCod)
#   }
#   
#   # Remove unnecessary data
#   v_remove <- names(csmf_malaria_01to04HMM)[!(names(csmf_malaria_01to04HMM) %in% idVars)]
#   dat <- dat[, !(names(dat) %in% v_remove)]
#   
#   # Add back LMM
#   dat <- rbind(dat, datLMM)
#   
#   # Tidy up
#   dat <- dat[order(dat$ISO3, dat$Year),]
#   
#   return(dat)
#   
# }


##################################################
####
####   Format predicted fractions for point estimates
####
##################################################


#L_DAT_HMM <- l_draws_HMM
#L_DAT_LMM <- l_draws_LMM
#UNCERTAINTY = TRUE

#L_DAT <- l_csmf_HMM
#DAT <- l_csmf_HMM[[1]]
#L_DAT <- l_draws_LMM

#testcsmf <- lapply(L_DAT, fn_format_prediction)

fn_format_prediction <- function(L_DAT_HMM, L_DAT_LMM){
  
  datHMM <- do.call(rbind, L_DAT_HMM)
  datLMM <- do.call(rbind, L_DAT_LMM)
  dat <- rbind(datHMM, datLMM)
  
  # Rearrange columns
  dat <- dat[, c(idVars, sort(names(dat)[which(!names(dat) %in% idVars)]))] 
  
  # Tidy up
  dat <- dat[order(dat$ISO3, dat$Year),]
  
  return(dat)
  
}

# fn_format_prediction <- function(DAT, UNCERTAINTY = FALSE){
#   
#   if(!UNCERTAINTY){
#     # Add column for sex
#     DAT$Sex <- sexLabel
#     # Add empty "Maternal" column for 15-19 males
#     if(sexLabel == sexLabels[3]){DAT$Maternal <- 0}
#     # Rename predicted CODs
#     # Note: Use these names in model estimation next round so there's no need to adjust.
#     names(DAT)[names(DAT) == "OtherCD"] <- "OtherCMPN"
#     names(DAT)[names(DAT) == "RTA"] <- "RTI"
#     names(DAT)[names(DAT) == "Self_harm"] <- "SelfHarm"
#     names(DAT)[names(DAT) == "Interp_violence"] <- "InterpVio"
#     names(DAT)[names(DAT) == "Other_inj"] <- "OtherInj"
#     # Rearrange columns
#     res <- DAT[, c(idVars, sort(names(DAT)[which(!names(DAT) %in% idVars)]))]
#     
#   }else{
#     # For uncertainty, perform operations on list
#     l_dat <- DAT
#     # Add column for sex
#     l_dat <- lapply(l_dat, function(x){ x$Sex <- sexLabel ; return(x)})
#     # Add empty "Maternal" column for 15-19 males
#     if(sexLabel == sexLabels[3]){l_dat <- lapply(l_dat, function(x){ x$Maternal <- 0 ; return(x)})}
#     # Rename predicted CODs
#     # !!! Note: Use these names in model estimation next round so there's no need to adjust.
#     l_dat <- lapply(l_dat, function(x){ names(x)[names(x) == "OtherCD"] <- "OtherCMPN"
#                                         names(x)[names(x) == "RTA"] <- "RTI"
#                                         names(x)[names(x) == "Self_harm"] <- "SelfHarm"
#                                         names(x)[names(x) == "Interp_violence"] <- "InterpVio"
#                                         names(x)[names(x) == "Other_inj"] <- "OtherInj" ; return(x)})
#     # Rearrange columns
#     res <- lapply(l_dat, function(x){ x <- x[, c(idVars, sort(names(x)[which(!names(x) %in% idVars)]))]; return(x)})
#   }
# 
#   return(res)
# }

# fn_format_prediction <- function(L_DAT, UNCERTAINTY = FALSE){
#   
#   if(!UNCERTAINTY){
#     # For point estimates, use regular lapply
#     fn_mylapply <- function(data, fun){ lapply(data, fun)}
#   }else{
#     # For uncertainty, use nested lapply for list of lists
#     fn_mylapply <- function(data, fun){ lapply(data, function(sublist) { lapply(sublist, fun) })}
#   }
#   
#   # Add column for sex
#   L_DAT <- fn_mylapply(L_DAT, function(x){ x$Sex <- sexLabel ; return(x)})
#   
#   # Add empty "Maternal" column for 15-19 males
#   if(sexLabel == sexLabels[3]){L_DAT <- fn_mylapply(L_DAT, function(x){ x$Maternal <- 0 ; return(x)})}
#   
#   # Rename predicted CODs
#   # !!! Note: Use these names in model estimation next round so there's no need to adjust.
#   L_DAT <- fn_mylapply(L_DAT, function(x){ names(x)[names(x) == "OtherCD"] <- "OtherCMPN"
#                                            names(x)[names(x) == "RTA"] <- "RTI"
#                                            names(x)[names(x) == "Self_harm"] <- "SelfHarm"
#                                            names(x)[names(x) == "Interp_violence"] <- "InterpVio"
#                                            names(x)[names(x) == "Other_inj"] <- "OtherInj" ; return(x)})
#   
#   # Rearrange columns
#   L_DAT <- fn_mylapply(L_DAT, function(x){ x <- x[, c(idVars, sort(names(x)[which(!names(x) %in% idVars)]))]; return(x)})
#   
#   # Set format
#   if(!UNCERTAINTY){
#     # For point estimates, return as dataframe
#     dat <- do.call(rbind, L_DAT)
#     dat <- dat[order(dat$ISO3, dat$Year),]
#   }else{
#     # For uncertainty, return as list of lists
#     dat <- L_DAT
#   }
#   
#   return(dat)
# }


# fn_format_prediction <- function(L_DAT_HMM, L_DAT_LMM, UNCERTAINTY = FALSE){
#   
#   # Set malaria as zero for LMM
#   l_dat_LMM <- lapply(L_DAT_LMM, function(x){ x$Malaria <- 0 ;return(x)})
#   
#   # Combine HMM and LMM
#   if(!UNCERTAINTY){
#     datHMM <- do.call(rbind, L_DAT_HMM)
#     datLMM <- do.call(rbind, l_dat_LMM)
#     # Save as list so same function can be used for both point estimates and draws
#     l_dat <- list(rbind(datHMM, datLMM))
#   }else{
#     l_dat <- Map(rbind, L_DAT_HMM, l_dat_LMM)
#   }
#   
#   # Add column for sex
#   l_dat <- lapply(l_dat, function(x){ x$Sex <- sexLabel ; return(x)})
#   
#   # Add empty "Maternal" column for 15-19 males
#   if(sexLabel == sexLabels[3]){l_dat <- lapply(l_dat, function(x){ x$Maternal <- 0 ; return(x)})}
#   
#   # Rename predicted CODs
#   # !!! Note: Use these names in model estimation next round so there's no need to adjust.
#   l_dat <- lapply(l_dat, function(x){ names(x)[names(x) == "OtherCD"] <- "OtherCMPN"
#                                       names(x)[names(x) == "RTA"] <- "RTI"
#                                       names(x)[names(x) == "Self_harm"] <- "SelfHarm"
#                                       names(x)[names(x) == "Interp_violence"] <- "InterpVio"
#                                       names(x)[names(x) == "Other_inj"] <- "OtherInj" ; return(x)})
#   
#   # Tidy up
#   l_dat <- lapply(l_dat, function(x){ x <- x[, c(idVars, sort(names(x)[which(!names(x) %in% idVars)]))]
#                                       x <- x[order(x$ISO3, x$Year),]; return(x)})
#   
#   if(!UNCERTAINTY){
#     dat <- l_dat[[1]]
#   }else{
#     dat <- l_dat
#   }
#   
#   return(dat)
# }


# fn_format_prediction <- function(l_dat_HMM, l_dat_LMM){
#   
#   # Transform list elements into data frame
#   datHMM <- do.call(rbind, l_dat_HMM)
#   datLMM <- do.call(rbind, l_dat_LMM)
#   
#   # Set malaria as zero for LMM
#   datLMM$Malaria <- 0
#   
#   # Combine HMM and LMM
#   dat <- rbind(datHMM, datLMM)
#   
#   # Add column for sex
#   dat$Sex <- sexLabel
#   # Add empty "Maternal" column for 15-19 males
#   if(sexLabel == sexLabels[3]){dat$Maternal <- 0}
#   
#   # Rename predicted CODs
#   # Note: Use these names in model estimation next round so there's no need to adjust.
#   names(dat)[names(dat) == "OtherCD"] <- "OtherCMPN"
#   names(dat)[names(dat) == "RTA"] <- "RTI"
#   names(dat)[names(dat) == "Self_harm"] <- "SelfHarm"
#   names(dat)[names(dat) == "Interp_violence"] <- "InterpVio"
#   names(dat)[names(dat) == "Other_inj"] <- "OtherInj"
#   
#   # Tidy up
#   dat <- dat[, c(idVars, sort(names(dat)[which(!names(dat) %in% idVars)]))]
#   dat <- dat[order(dat$ISO3, dat$Year),]
#   
#   return(dat)
# }

