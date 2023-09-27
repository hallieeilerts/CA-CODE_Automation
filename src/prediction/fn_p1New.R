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
