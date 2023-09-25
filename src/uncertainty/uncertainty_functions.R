
fn_nested_lapply <- function(X, FUN){ 
  
  #' @title Nested lapply()
  # 
  #' @description Performs lapply on list of lists
  #
  #' @param X List of lists.
  #' @param FUN Function to be performed on nested list elements.
  #' @return List of lists with second-level elements altered by FUN.
  
  lapply(X, function(sublist) { lapply(sublist, FUN) }) 
}

fn_rearrange_draws <- function(L_CSMFDRAWS){
  
  #' @title Rearrange draws of predicted fractions
  # 
  #' @description Transforms nested list (first level = year, second level = draws) into a list 
  #' where each element is a draw which includes all predicted CSMFs for that year.
  #
  #' @param L_CSMFDRAWS List of length number of years being predicted.
  #' Each first-level list element is a list of length number of draws.
  #' Within first-level list element, each second-level list element is a data frame with predicted CSMFs for one year.
  #' @return List of length number of draws.
  #' Each list element is a data frame with predicted CSMFs for one draw.
  
  # Add sequential name for draws
  # Transform list of all draws into a data frame with an index column for "draw"
  l_draws <- lapply(L_CSMFDRAWS, function(x){ names(x) <- 1:length(x)
                                          x <- ldply(x, .id = "draw")
                                          return(x)})
  df_draws <- ldply(l_draws)
  
  # Split into list by "draw" column
  l_draws <- split(df_draws, df_draws$draw)
  l_draws <- lapply(l_draws, function(x){ x <- x[order(x$ISO3, x$Year),]})
  
  # Tidy up
  l_draws <- lapply(l_draws, function(x){x$draw <- NULL ; return(x)})
  l_draws <- lapply(l_draws, function(x){ x[, c(idVars, sort(names(x)[which(!names(x) %in% idVars)]))] ; return(x)})
  l_draws <- lapply(l_draws, function(x){ rownames(x) <- NULL ; return(x)})
  
  return(l_draws)
}

fn_create_sample_vectors <- function(L_CSMFDRAWS_HMM, L_CSMFDRAWS_LMM, L_ENVDRAWS){
  
  #' @title Create randomly sampled vectors for igme draws and predicted fractions
  # 
  #' @description Randomly samples draws from envelopes and predicted LMM and HMM fractions.
  #' Saves sample vectors in a list.
  #
  #' @param L_CSMFDRAWS_HMM List of length number of draws.
  #' Each list element is a data frame with CSMFs for all years being predicted for HMM countries.
  #' @param L_CSMFDRAWS_LMM List of length number of draws.
  #' Each list element is a data frame with CSMFs for all years being predicted for LMM countries.
  #' @param L_ENVDRAWS List of length three, corresponding to crisis-free deaths, crisis-included deaths, crisis-included rates.
  #' Each first-level list element is a list of length number of draws.
  #' Within first-level list element, each second-level list element is a data frame 
  #' with a draw with columns c('ISO3', 'Year', 'Sex', 'Deaths1', 'Deaths2', 'Rate').
  #' @return List of length three, corresponding to envelope, HMM, LMM.
  #' Each list element is a vector (all of equal length) with a list of integers.
  #' Integers are randomly sampled draws from envelope, HMM, LMM.
  
  if(length(L_CSMFDRAWS_HMM) >= length(L_CSMFDRAWS_LMM)){
    # Sample from IGME draws, number of samples = number of draws for predicted fractions
    #v_sample_env <- sort(sample(x = dim(L_ENVDRAWS$deaths)[3], size = length(L_CSMFDRAWS_HMM))) 
    v_sample_env <- sort(sample(x = length(L_ENVDRAWS$deaths1), size = length(L_CSMFDRAWS_HMM))) 
    
    # Sets of HMM fractions
    v_sample_HMM <- 1:length(L_CSMFDRAWS_HMM)
    
    # Sets of LMM fractions
    # With extra samples if there are fewer LMM draws than HMM
    v_sample_LMM <- c(1:length(L_CSMFDRAWS_LMM),
                      sort(sample(x = 1:length(L_CSMFDRAWS_LMM),
                                  size = length(L_CSMFDRAWS_HMM) - length(L_CSMFDRAWS_LMM), replace = T)))
  }else{
    
    # Sample from IGME draws, number of samples = number of draws for predicted fractions
    #v_sample_env <- sort(sample(x = dim(L_ENVDRAWS$deaths)[3], size = length(L_CSMFDRAWS_LMM)))
    v_sample_env <- sort(sample(x = length(L_ENVDRAWS$deaths1), size = length(L_CSMFDRAWS_LMM))) 
    
    # Sets of HMM fractions
    # With extra samples if there are fewer HMM draws than LMM
    v_sample_HMM <- c(1:length(L_CSMFDRAWS_HMM),
                      sort(sample(x = 1:length(L_CSMFDRAWS_HMM),
                                  size = length(L_CSMFDRAWS_LMM) - length(L_CSMFDRAWS_HMM), replace = T)))
    
    # Sets of LMM fractions
    v_sample_LMM <- 1:length(L_CSMFDRAWS_LMM)
  }
  
  # Combine all sample vectors into list
  v_sample <- list(v_sample_env, v_sample_HMM, v_sample_LMM)
  names(v_sample) <- c("env", "HMM", "LMM")
  
  return(v_sample)
}

fn_rand_draw_env <- function(L_ENVDRAWS, V_SAMPLE){
  
  #' @title Random draw from IGME envelopes with sample vector
  # 
  #' @description Indexes randomly sampled draws from envelope. Merges crisis-free/included draws together.
  #' 
  #' @param L_ENVDRAWS List of length three, corresponding to crisis-free deaths, crisis-included deaths, crisis-included rates.
  #' Each first-level list element is a list of length number of draws.
  #' Within first-level list element, each second-level list element is one envelope draw:
  #" a data frame with columns c("ISO3", "Year", "Sex", "Deaths1", "Deaths2", "Rate").
  #' @param V_SAMPLE List of length three, corresponding to envelope, HMM, LMM.
  #' Each list element is a vector (all of equal length) with a list of integers.
  #' Integers are randomly sampled draws from envelope, HMM, LMM.
  #' @return List of length that is the same as number of V_SAMPLE integers.
  #' Each list element is one envelope draw: a data frame with columns c("ISO3", "Year", "Sex", "Deaths1", "Deaths2", "Rate").
  
  # Crisis-free IGME deaths (random draw)
  l_deaths1 <- lapply(V_SAMPLE, function(x){ L_ENVDRAWS$deaths1[[x]] })
  
  # Crisis-included IGME deaths (random draw)
  l_deaths2 <- lapply(V_SAMPLE, function(x){ L_ENVDRAWS$deaths2[[x]] })
  
  # Crisis-included IGME rates (random draw)
  l_rates2 <- lapply(V_SAMPLE, function(x){ L_ENVDRAWS$rates2[[x]] })
  
  # Merge
  l_draws_samp <- mapply(function(x, y) merge(x, y, by = idVars, all=TRUE), x = l_deaths1, y = l_deaths2, SIMPLIFY = FALSE)
  l_draws_samp <- mapply(function(x, y) merge(x, y, by = idVars, all=TRUE), x = l_draws_samp, y = l_rates2, SIMPLIFY = FALSE)
  
  # Tidy up
  l_draws_samp <- lapply(l_draws_samp, function(x){ x[order(x$ISO3, x$Year),] })
  l_draws_samp <- lapply(l_draws_samp, function(x){ x[, c(idVars, sort(names(x)[!(names(x) %in% idVars)]))] })
  
  return(l_draws_samp)
}

fn_format_draws <- function(L_CSMFDRAWS_HMM, L_CSMFDRAWS_LMM){
  
  #' @title Format draws of predicted fractions
  # 
  #' @description Combines randomly sampled draws for HMM and LMM.
  #' 
  #' @param L_CSMFDRAWS_HMM List of length number of draws.
  #' Each list element is a data frame with CSMFs for all years being predicted for HMM countries.
  #' @param L_CSMFDRAWS_LMM List of length number of draws.
  #' Each list element is a data frame with CSMFs for all years being predicted for LMM countries.
  #' @return List of length number of draws.
  #' Each list element is a data frame with predicted CSMFs for HMM and LMM for one draw.
  
  l_draws <- mapply(rbind, L_CSMFDRAWS_HMM, L_CSMFDRAWS_LMM, SIMPLIFY=FALSE)

  # Rearrange columns
  l_draws <- lapply(l_draws, function(x){ x[, c(idVars, sort(names(x)[which(!names(x) %in% idVars)]))] })

  # Tidy up
  l_draws <- lapply(l_draws, function(x){ x[order(x$ISO3, x$Year),] })
  
  return(l_draws)
  
}

fn_rand_assign_vr <- function(CSMF, KEY_COD, CTRYGRP){
  
  #' @title Randomly assign CSMF values for goodvr/China for current draw
  # 
  #' @description Sample from multinomial distribution to perturb CSMFs for goodVR/China for current draw.
  #' 
  #' @param CSMF Data frame with CSMFs for goodvr/China.
  #' @param KEY_COD Data frame with age-specific CODs with different levels of classification.
  #' @param CTRYGRP Character string that must be set as either 'GOODVR' or 'CHN'.
  #' @return Data frame with randomly sampled CSMFs.
  
  if(!(CTRYGRP %in% c("GOODVR", "CHN"))){
    stop("Must set CTRYGRP as either GOODVR or CHN")
  }
  
  dat <- CSMF

  # Vector with all causes of death (including single-cause estimates)
  v_cod <- unique(KEY_COD$Reclass)  
  v_cod <- v_cod[!v_cod %in% c("Other", "Undetermined")]
  # If China, also exclude HIV as this will be added through squeezing
  if(CTRYGRP == "CHN"){ 
    v_cod <- v_cod[v_cod != "HIV"]
  }
  
  # Random CAUSE-SPECIFIC deaths from multinomial distribution
  dat[, paste(v_cod)] <- t(apply(dat[, c(v_cod, "Deaths2")], 1,
                                  function(x) {
                                    rmultinom(n = 1, size = round(x["Deaths2"]),
                                              prob = x[paste(v_cod)])
                                  }))
  
  # Transform into fractions
  dat[, paste(v_cod)] <- dat[, paste(v_cod)] / rowSums(dat[, paste(v_cod)])
  
  # Return random draw for GOODVR or CHN
  return(dat)

}

fn_sample_log_norm <- function(MU, LB, UB) {
  
  #' @title Sample from log-normal distribution
  # 
  #' @description Sample from log-normal distribution.
  #' 
  #' @param MU Integer that is the mean value (CSMFpoint estimate).
  #' @param LB Integer that is  the lower bound of the 95% UI.
  #' @param UB Integer that is  the upper bound of the 95% UI.
  #' @return Integer that has been randomly sampled from log-normal distribution
  
  # Estimate normal SD
  s <- (UB - LB) / 3.92
  # Log-normal mean
  muLog <- log(MU^2 / sqrt(s^2 + MU^2))
  # Log-normal sd
  sLog <- sqrt(log(1 + (s^2 / MU^2)))
  # Draw random values
  randVal <- rlnorm(n = length(MU), meanlog = muLog, sdlog = sLog)
  
  # Output
  return(randVal)
  
}

fn_rand_assign_meas <- function(CSMFDRAW){

  #' @title Randomly assign measles deaths for current draw
  # 
  #' @description Cap total measles deaths (endemic + epidemic) to crisis-included envelope.
  #' If total measles deaths > 0, sample new total from log normal distribution.
  #' If endemic measles deaths > 0, recalculate endemic by subtracting original epidemic measles from randomly sampled total.
  #' If epidemic measles deaths > 0, recalculate epidemic by subtracting original endemic measles from randomly sampled total.
  #' 
  #' @param CSMFDRAW Data frame that is one draw of predicted CSMFs, single cause data, envelopes, and minimum fractions.
  #' @return Data frame with randomly sampled measles deaths.
  
  dat <- CSMFDRAW
  
  # Cap the upper bound of Measles (EPI + END) to the envelope
  dat$msl_ub[which(dat$msl_ub > dat$Deaths2)] <- dat$Deaths2[which(dat$msl_ub > dat$Deaths2)]
  
  # Uncertainty intervals only available for endemic + epidemic Measles
  # Random epidemic + endemic Measles
  
  # Randomly sample value for total measles
  v_msl <- rep(0, nrow(dat))
  v_idSqz <- which(dat$msl != 0)
  if(length(v_idSqz) > 0){
    # Sample from log normal distribution
    v_msl[v_idSqz] <- fn_sample_log_norm(MU = dat$msl[v_idSqz],
                                         LB = dat$msl_lb[v_idSqz],
                                         UB = dat$msl_ub[v_idSqz])
    # Save point estimate for endemic measles
    v_meas_end <- dat$Measles
    v_idSqz <- which(dat$Measles != 0)
    # Calculate random endemic measles by subtracting epidemic from random total
    dat$Measles[v_idSqz] <- v_msl[v_idSqz] - dat$meas_epi[v_idSqz] 
    # Avoid negative values
    dat$Measles[which(dat$Measles < 0)] <- 0
    # Calculate random epidemic measles by subtracting endemic point estimate from random total
    v_idSqz <- which(dat$meas_epi != 0)
    dat$meas_epi[v_idSqz] <- v_msl[v_idSqz] - v_meas_end[v_idSqz]
    # Note: Measles and meas_epi do not need to add up to msl, but their total should be within msl_lb and msl_ub
    ## meas_epi will be added on top of envelope
  }
  
  return(dat)
  
}

fn_rand_assign_tb <- function(CSMFDRAW){
  
  #' @title Randomly assign TB deaths for current draw
  # 
  #' @description Cap TB deaths (TB and TBre) to crisis-included envelope.
  #' If TB deaths != 0 and is within lower and upper TB bounds, sample from log normal distribution.
  #' 
  #' @param CSMFDRAW Data frame that is one draw of predicted CSMFs, single cause data, envelopes, and minimum fractions.
  #' @return Data frame with randomly sampled TB deaths.
  
  dat <- CSMFDRAW
  
  # Cap the upper bound of TB to the envelope
  dat$tb_ub[which(dat$tb_ub > dat$Deaths1)] <- dat$Deaths1[which(dat$tb_ub > dat$Deaths1)]
  # Values to be squeezed
  v_idSqz <- which(dat$TB != 0 & dat$TB > dat$tb_lb & dat$TB < dat$tb_ub)
  # Sample random values
  if (length(v_idSqz) > 0) {
    dat$TB[v_idSqz] <- fn_sample_log_norm(MU = dat$TB[v_idSqz],
                                          LB = dat$tb_lb[v_idSqz],
                                          UB = dat$tb_ub[v_idSqz])
  }
  
  if(respTB){
    
    # Cap the upper bound of TB to the envelope
    dat$tbre_ub[which(dat$tbre_ub > dat$Deaths1)] <- dat$Deaths1[which(dat$tbre_ub > dat$Deaths1)]
    # Values to be squeezed
    v_idSqz <- which(dat$TBre != 0 & dat$TBre > dat$tbre_lb & dat$TBre < dat$tbre_ub)
    # Sample random values
    if (length(v_idSqz) > 0) {
      dat$TBre[v_idSqz] <- fn_sample_log_norm(MU = dat$TBre[v_idSqz],
                                              LB = dat$tbre_lb[v_idSqz],
                                              UB = dat$tbre_ub[v_idSqz])
    }
  }
  
  return(dat)
}

fn_rand_assign_hiv <- function(CSMFDRAW){
  
  #' @title Randomly assign HIV deaths for current draw
  # 
  #' @description If HIV deaths != 0 and is within lower and upper HIV bounds, sample from log normal distribution.
  #' 
  #' @param CSMFDRAW Data frame that is one draw of predicted CSMFs, single cause data, envelopes, and minimum fractions.
  #' @return Data frame with randomly sampled HIV deaths.
  
  dat <- CSMFDRAW
  
  # Values to be squeezed
  v_idSqz <- which(dat$HIV != 0 & dat$HIV > dat$hiv_lb & dat$HIV < dat$hiv_ub)
  
  # Sample random values
  if (length(v_idSqz) > 0) {
    dat$HIV[v_idSqz] <- rtnorm(n = length(v_idSqz), mean = dat$HIV[v_idSqz],
                               sd = (dat$hiv_ub[v_idSqz] - dat$hiv_lb[v_idSqz]) / 3.92,
                               lower = 0)
  }
  
  return(dat)
}

fn_rand_assign_crisisend <- function(CSMFDRAW){
  
  #' @title Randomly assign endemic crisis deaths for current draw
  # 
  #' @description If there are any endemic crisis deaths, sample from multinomial distribution.
  #' 
  #' @param CSMFDRAW Data frame that is one draw of predicted CSMFs, single cause data, envelopes, and minimum fractions.
  #' @return Data frame with randomly sampled endemic crisis deaths.
  
  dat <- CSMFDRAW
  
  # Add crisis-free deaths with endemic CollectVio and NatDis
  v_deaths <- dat$Deaths1 + dat$CollectVio + dat$NatDis
  
  # Calculate fraction of endemic collective violence (Pro-rata squeeze)
  dat$CollectVio <- dat$CollectVio/v_deaths
  
  # Calculate fraction of endemic natural disaster (Pro-rata squeeze)
  dat$NatDis <- dat$NatDis/v_deaths
  
  # Values to be squeezed
  v_idSqz <- which(dat$CollectVio != 0 | dat$NatDis != 0)
  
  # Sample random values
  if (length(v_idSqz) > 0) {
    # Create data.frame with csmfs for inverse of endemic crisis, CollectVio, NatDis, and crisis-free deaths
    datAux <- cbind(1 - dat$CollectVio[v_idSqz] - dat$NatDis[v_idSqz],
                    dat$CollectVio[v_idSqz], dat$NatDis[v_idSqz], 
                    dat$Deaths1[v_idSqz])
    # Randomly sample from crisis-free deaths with probability equivalent to the three CSMFs
    datAux <- t(apply(datAux, 1,
                      function(x) {
                        rmultinom(n = 1, size = round(x[4]), prob = x[1:3])
                     }))
    # Update the Collective Violence and Natural Disasters fractions for the current draw
    dat$CollectVio[v_idSqz] <- datAux[, 2]/dat$Deaths1[v_idSqz]
    dat$NatDis[v_idSqz] <- datAux[, 3]/dat$Deaths1[v_idSqz]
  }
  
  return(dat)
}

fn_calc_ui <- function(L_CSMFDRAWS, UI, CODALL, ENV = NULL, REGIONAL = FALSE){
  
  #' @title Calculate uncertainty intervals for fractions/rates/deaths from draws
  # 
  #' @description Create a separate list for fractions/rates/deaths.
  #' Convert each list of data frames to an array.
  #' Calculate the quantile for each cell across the matrices of the array.
  #' Save matrices with quantiles as data frames with identifying columns.
  #' Combine all data frames.
  #
  #' @param L_CSMFDRAWS List of length number of draws of predicted fractions.
  #' Each list element is a data frame with CSMFs for every country-year for all CODs estimated.
  #' Also contains columns c("ISO3", "Year", "Sex", "Deaths", "Rate")
  #' @param ENV
  #' @param UI Integer with the width of the uncertainty interval desired.
  #' @param KEY_COD Data frame with age-specific CODs with different levels of classification.
  #' @return Data frame with lower and upper quantiles for each COD for deaths, fractions, rates.
  
  # Create interval
  UI <- 1/2 + c(-UI, UI) / 2
  
  # Causes of death for this age group
  v_cod <- CODALL[CODALL %in% names(L_CSMFDRAWS[[1]])]
  # Causes of death for this age group
  #v_cod <- unique(KEY_COD$Reclass)  # Vector with ALL CAUSES OF DEATH (including single-cause estimates)
  #v_cod <- v_cod[!v_cod %in% c("Other", "Undetermined")]
  #v_cod <- v_cod[v_cod %in% names(L_CSMFDRAWS[[1]])]
  
  # One data frame with identifying columns that are shared across all draws
  df_idcols <- L_CSMFDRAWS[[1]][, !names(L_CSMFDRAWS[[1]]) %in% c("Deaths1", "Rate1", "Deaths2", "Rate2", paste(v_cod))]
  
  if(!is.null(ENV)){
    # All-cause rate from IGME envelope (for use in 2023.06.14 PATCH below)
    df_env <- merge(df_idcols, ENV[,c(idVars, "Rate2")], all.x = TRUE)
  }
  
  # Create lists for draws of fractions, rates, deaths
  l_frac   <- L_CSMFDRAWS
  l_rates  <- lapply(L_CSMFDRAWS, function(x){ x[,v_cod] <- x[,v_cod] * x[,"Rate2"] ; return(x)})
  l_deaths <- lapply(L_CSMFDRAWS, function(x){ x[,v_cod] <- x[,v_cod] * x[,"Deaths2"] ; return(x)})
  
  # Convert each data.frame in list to matrix that includes COD columns and all-cause deaths and rates
  l_frac   <- lapply(l_frac, function(x) as.matrix(x[,c("Deaths2", "Rate2", v_cod)]))
  l_deaths <- lapply(l_deaths, function(x) as.matrix(x[,c("Deaths2", "Rate2", v_cod)]))
  l_rates  <- lapply(l_rates, function(x) as.matrix(x[,c("Deaths2", "Rate2", v_cod)]))
  
  # Convert lists to arrays
  # Calculate quantiles for each cell across matrices of the array
  m_frac_lb <- apply(simplify2array(l_frac), c(1,2), quantile, UI[1], na.rm = T)
  m_frac_ub <- apply(simplify2array(l_frac), c(1,2), quantile, UI[2], na.rm = T)
  m_deaths_lb <- apply(simplify2array(l_deaths), c(1,2), quantile, UI[1], na.rm = T)
  m_deaths_ub <- apply(simplify2array(l_deaths), c(1,2), quantile, UI[2], na.rm = T)
  m_rates_lb <- apply(simplify2array(l_rates), c(1,2), quantile, UI[1], na.rm = T)
  m_rates_ub <- apply(simplify2array(l_rates), c(1,2), quantile, UI[2], na.rm = T)
  
  if(!is.null(ENV)){
    #------------------------#
    # 2023.06.14 PATCH
    # Adjust upper bound of all-cause mortality rate (and cause specific rates) if it is ever below IGME envelope point estimate.
    # From Pancho's function CalcUncert(). 
    # Check if the upper bound of the all-cause rate (column 2) is below IGME envelope.
    # If so, shift cause-specific and all-cause rate upper bound upwards for that country-year in all draws.
    # Recalculate quantiles for upper bounds.
    # Check again.
    idqx <- which(m_rates_ub[,2] < df_env$Rate2)
    idqx_check <- idqx
    if (length(idqx) > 0) {
      l_rates_Aux         <- lapply(l_rates, function(x) as.matrix(x[idqx, c("Deaths2","Rate2", v_cod)])  * 1.05 )
      m_rates_ub_Aux      <- apply(simplify2array(l_rates_Aux), c(1,2), quantile, .99, na.rm = T)
      m_rates_ub[idqx, ]  <- m_rates_ub_Aux
    }
    idqx <- which(m_rates_ub[,2] < df_env$Rate2)
    if (length(idqx) > 0) {
      l_rates_Aux         <- lapply(l_rates, function(x) as.matrix(x[idqx, c("Deaths2","Rate2", v_cod)])  * 1.1 )
      m_rates_ub_Aux      <- apply(simplify2array(l_rates_Aux), c(1,2), quantile, .99, na.rm = T)
      m_rates_ub[idqx, ]  <- m_rates_ub_Aux
    }
    idqx <- which(m_rates_ub[,2] < df_env$Rate2)
    if (length(idqx) > 0) {
      l_rates_Aux         <- lapply(l_rates, function(x) as.matrix(x[idqx, c("Deaths2","Rate2", v_cod)])  * 1.5 )
      m_rates_ub_Aux      <- apply(simplify2array(l_rates_Aux), c(1,2), quantile, .99, na.rm = T)
      m_rates_ub[idqx, ]  <- m_rates_ub_Aux
    }
    idqx <- which(m_rates_ub[,2] < df_env$Rate2)
    if (length(idqx) > 0) {
      l_rates_Aux         <- lapply(l_rates, function(x) as.matrix(x[idqx, c("Deaths2","Rate2", v_cod)])  * 2 )
      m_rates_ub_Aux      <- apply(simplify2array(l_rates_Aux), c(1,2), quantile, .99, na.rm = T)
      m_rates_ub[idqx, ]  <- m_rates_ub_Aux
    }
    idqx <- which(m_rates_ub[,2] < df_env$Rate2)
    if (length(idqx) > 0) {
      l_rates_Aux         <- lapply(l_rates, function(x) as.matrix(x[idqx, c("Deaths2","Rate2", v_cod)])  * 3 )
      m_rates_ub_Aux      <- apply(simplify2array(l_rates_Aux), c(1,2), quantile, .99, na.rm = T)
      m_rates_ub[idqx, ]  <- m_rates_ub_Aux
    }
    idqx <- which(m_rates_ub[,2] < df_env$Rate2)
    if (length(idqx) > 0) {
      l_rates_Aux         <- lapply(l_rates, function(x) as.matrix(x[idqx, c("Deaths2","Rate2", v_cod)])  * 4.5 )
      m_rates_ub_Aux      <- apply(simplify2array(l_rates_Aux), c(1,2), quantile, .999, na.rm = T)
      m_rates_ub[idqx, ]  <- m_rates_ub_Aux
    }
    # END PATCH
    #------------------------#
  }
  
  # Format arrays in to data frames
  df_frac_lb <- as.data.frame(cbind(df_idcols, 
                                    Variable = rep("Fraction", nrow(df_idcols)),
                                    Quantile = rep("Lower", nrow(df_idcols)), 
                                    m_frac_lb))
  df_frac_ub <- as.data.frame(cbind(df_idcols, 
                                    Variable = rep("Fraction", nrow(df_idcols)),
                                    Quantile = rep("Upper", nrow(df_idcols)), 
                                    m_frac_ub))
  df_deaths_lb <- as.data.frame(cbind(df_idcols, 
                                      Variable = rep("Deaths", nrow(df_idcols)),
                                      Quantile = rep("Lower", nrow(df_idcols)), 
                                      m_deaths_lb))
  df_deaths_ub <- as.data.frame(cbind(df_idcols, 
                                      Variable = rep("Deaths", nrow(df_idcols)),
                                      Quantile = rep("Upper", nrow(df_idcols)), 
                                      m_deaths_ub))  
  df_rates_lb <- as.data.frame(cbind(df_idcols, 
                                     Variable = rep("Rate", nrow(df_idcols)),
                                     Quantile = rep("Lower", nrow(df_idcols)), 
                                     m_rates_lb))
  df_rates_ub <- as.data.frame(cbind(df_idcols, 
                                     Variable = rep("Rate", nrow(df_idcols)),
                                     Quantile = rep("Upper", nrow(df_idcols)), 
                                     m_rates_ub))
  
  # Combine and tidy
  df_res <- rbind(df_frac_lb, df_frac_ub, df_deaths_lb, df_deaths_ub, df_rates_lb, df_rates_ub)
  if(!REGIONAL){
    df_res <- df_res[order(df_res$ISO3, df_res$Year, df_res$Sex, df_res$Variable, df_res$Quantile),]
  }else{
    df_res <- df_res[order(df_res$Region, df_res$Year, df_res$Sex, df_res$Variable, df_res$Quantile),]
  }
  rownames(df_res) <- NULL
  
  # Checking 2023.06.14 PATCH adjustment
  # idqx_check
  # 421  422 1387 1388 3763 4008 4009 4011 4012 4013 4014 4015
  #m_rates_ub[421,]
  #df_idcols[421,]
  #subset(df_env, ISO3 == "BIH" & Year == 2002)
  #subset(df_res, ISO3 == "BIH" & Year == 2002)
  
  return(df_res)
}

#CSMFSQZ <- csmfSqz_AGG_05to19
#UI <- ui_05to19
#CODALL <- codAll

fn_combine_ui_point <- function(UI, CSMFSQZ, CODALL, REGIONAL = FALSE){
  
  #' @title Combine raw data frames for CSMF point estimates and uncertainty intervals.
  # 
  #' @description Transforms CSMF point estimates into rates and deaths as well, combines with uncertainty intervals for CSMFs, rates, and deaths, orders rows and columns.
  #
  #' @param CSMFSQZ Data frame with CSMFs that have been processed in squeezing pipeline (contains all countries, even those not subject to squeezing).
  #' @param UI     Data frame with lower and upper quantiles for each COD for deaths, fractions, rates.
  #' @param KEY_COD Data frame with age-specific CODs with different levels of classification.
  #' @return Data frame with point estimates, lower, and upper bounds for CSMFs, deaths, and rates.
  
  # Causes of death for this age group
  v_cod <- CODALL[CODALL %in% names(CSMFSQZ)]
  #v_cod <- unique(KEY_COD$Reclass)  # Vector with ALL CAUSES OF DEATH (including single-cause estimates)
  #v_cod <- v_cod[!v_cod %in% c("Other", "Undetermined")]
  
  # Aux idVars
  if(!REGIONAL){
    idVarsAux <- idVars
  }else{
    idVarsAux <- c("Region", idVars[2:3])
  }
  
  # Only keep the following columns
  csmfSqz <- CSMFSQZ[,c(idVarsAux, "Deaths2", "Rate2", v_cod)]
  
  df_frac  <- csmfSqz
  df_rates <- csmfSqz
  df_deaths <- csmfSqz
  df_rates[,v_cod]  <- csmfSqz[,v_cod] * csmfSqz[,"Rate2"]
  df_deaths[,v_cod] <- csmfSqz[,v_cod] * csmfSqz[,"Deaths2"]
  
  # Add columns identifying point estimates
  df_frac$Variable   <- "Fraction"
  df_rates$Variable  <- "Rate"
  df_deaths$Variable <- "Deaths"
  df_frac$Quantile   <- "Point"
  df_rates$Quantile  <- "Point"
  df_deaths$Quantile <- "Point"
  
  # Keep same columns in UI
  v_col <- names(df_frac)[names(df_frac) %in% names(UI)]
  ui <- UI[,paste(v_col)]
  
  # Combine and tidy
  df_res <- rbind(df_frac, df_rates, df_deaths, ui)
  df_res <- df_res[, c(idVarsAux, "Deaths2", "Rate2", "Variable", "Quantile", v_cod)]
  if(!REGIONAL){
    df_res <- df_res[order(df_res$ISO3, df_res$Year, df_res$Sex, df_res$Variable, df_res$Quantile),]
  }else{
    df_res <- df_res[order(df_res$Region, df_res$Year, df_res$Sex, df_res$Variable, df_res$Quantile),]
  }
  rownames(df_res) <- NULL
  
  return(df_res)
  
}

fn_round_pointint <- function(POINTINT, CODALL, REGIONAL = FALSE){
  
  #' @title Round point estimates, lower, and upper bounds for fractions/deaths/and rates
  # 
  #' @description Rounds estimates, lower, and upper bounds for fractions/deaths/and rates
  #' When cause-specific deaths point estimate is between 0 and 1, change LB of fractions/rates/deaths to 0.
  #
  #' @param POINTINT Data frame with point estimates, lower, and upper bounds for fractions/deaths/and rates
  #' @param KEY_COD Data frame with age-specific CODs with different levels of classification.
  #' @param REGIONAL Boolean with true/false value if regional estimates.
  #' @return Data frame with rounded point estimates, lower, and upper bounds for fractions/deaths/and rates
  
  dat <- data.frame(POINTINT)
  
  # Causes of death for this age group
  v_cod <- CODALL[CODALL %in% names(dat)]
  #v_cod <- unique(KEY_COD$Reclass)  # Vector with ALL CAUSES OF DEATH (including single-cause estimates)
  #v_cod <- v_cod[!v_cod %in% c("Other", "Undetermined")]
  
  ## Deaths
  
  # Round quantiles for all-cause deaths
  dat$Deaths2[dat$Quantile == "Point"] <- round(dat$Deaths2[dat$Quantile == "Point"])
  dat$Deaths2[dat$Quantile == "Lower"] <- floor(dat$Deaths2[dat$Quantile == "Lower"])
  dat$Deaths2[dat$Quantile == "Upper"] <- ceiling(dat$Deaths2[dat$Quantile == "Upper"])
  
  # Round quantiles for cause-specific deaths
  dat[dat$Variable == "Deaths" & dat$Quantile == "Point", v_cod] <- 
    round(dat[dat$Variable == "Deaths" & dat$Quantile == "Point", v_cod])
  dat[dat$Variable == "Deaths" & dat$Quantile == "Lower", v_cod] <- 
    floor(dat[dat$Variable == "Deaths" & dat$Quantile == "Lower", v_cod])
  dat[dat$Variable == "Deaths" & dat$Quantile == "Upper", v_cod] <- 
    ceiling(dat[dat$Variable == "Deaths" & dat$Quantile == "Upper", v_cod])
  
  ## Rates
  
  # Round all-cause rates
  dat$Rate2[dat$Quantile == "Point"] <- round(dat$Rate2[dat$Quantile == "Point"], 5)
  dat$Rate2[dat$Quantile == "Lower"] <- floor(dat$Rate2[dat$Quantile == "Lower"]*10^5) / 10^5
  dat$Rate2[dat$Quantile == "Upper"] <- ceiling(dat$Rate2[dat$Quantile == "Upper"]*10^5) / 10^5
  
  # Round cause-specific rates
  dat[dat$Variable == "Rate" & dat$Quantile == "Point", v_cod] <- 
    round(dat[dat$Variable == "Rate" & dat$Quantile == "Point", v_cod], 5)
  dat[dat$Variable == "Rate" & dat$Quantile == "Lower", v_cod] <- 
    floor(dat[dat$Variable == "Rate" & dat$Quantile == "Lower", v_cod]*10^5) / 10^5
  dat[dat$Variable == "Rate" & dat$Quantile == "Upper", v_cod] <- 
    ceiling(dat[dat$Variable == "Rate" & dat$Quantile == "Upper", v_cod]*10^5) / 10^5
  
  ## Fractions
  
  # Round cause-specific fractions
  dat[dat$Variable == "Fraction" & dat$Quantile == "Point", v_cod] <- 
    round(dat[dat$Variable == "Fraction" & dat$Quantile == "Point", v_cod], 5)
  dat[dat$Variable == "Fraction" & dat$Quantile == "Lower", v_cod] <- 
    floor(dat[dat$Variable == "Fraction" & dat$Quantile == "Lower", v_cod]*10^5) / 10^5
  dat[dat$Variable == "Fraction" & dat$Quantile == "Upper", v_cod] <- 
    ceiling(dat[dat$Variable == "Fraction" & dat$Quantile == "Upper", v_cod]*10^5) / 10^5
  
  # Tidy
  if(!REGIONAL){
    dat <- dat[order(dat$ISO3, dat$Year, dat$Sex, dat$Variable, dat$Quantile),]
  }else{
    dat <- dat[order(dat$Region, dat$Year, dat$Sex, dat$Variable, dat$Quantile),]
  }
  rownames(dat) <- NULL
  
  return(dat)
  
}

fn_check_ui <- function(POINTINT, CODALL, REGIONAL = FALSE){
  
  #' @title Check if point estimates fall inside uncertainty intervals
  # 
  #' @description Checks for country-years where point estimate for deaths, fractions, or rates are outside of uncertainty intervals.
  #
  #' @param POINTINT Data frame with rounded point estimates, lower, and upper bounds for fractions/deaths/rates
  #' @param KEY_COD Data frame with age-specific CODs with different levels of classification.
  #' @return Data frame with rows where death/fraction/rate point estimates are outside of uncertainty intervals.
  
  dat <- POINTINT
  
  # Causes of death for this age group
  v_cod <- CODALL[CODALL %in% names(dat)]
  #v_cod <- unique(KEY_COD$Reclass)  # Vector with ALL CAUSES OF DEATH (including single-cause estimates)
  #v_cod <- v_cod[!v_cod %in% c("Other", "Undetermined")]
  
  # Aux idVars
  if(!REGIONAL){
    idVarsAux <- idVars
  }else{
    idVarsAux <- c("Region", idVars[2:3])
  }
  
  # Reshape to long
  datLong <- melt(setDT(dat), measure.vars = v_cod, variable.name = "cod")
  
  # Reshape Variable and Quantile wide
  if(!REGIONAL){
    datWide <- dcast(datLong,  ISO3+Year+Sex+cod ~ Variable+Quantile, value.var = "value")
  }else{
    datWide <- dcast(datLong,  Region+Year+Sex+cod ~ Variable+Quantile, value.var = "value")
  }
  
  # Check that point estimate is inside uncertainty interval
  datWide$Deaths_Check   <- ifelse(datWide$Deaths_Lower > datWide$Deaths_Point | datWide$Deaths_Upper < datWide$Deaths_Point, 1, 0)
  datWide$Fraction_Check <- ifelse(datWide$Fraction_Lower > datWide$Fraction_Point | datWide$Fraction_Upper < datWide$Fraction_Point, 1, 0)
  datWide$Rate_Check     <- ifelse(datWide$Rate_Lower > datWide$Rate_Point | datWide$Rate_Upper < datWide$Rate_Point, 1, 0)
  
  # Reshape to long
  datLong2 <- melt(datWide, id.vars = c(idVarsAux, "cod", "Deaths_Check", "Fraction_Check", "Rate_Check"))
  
  # Keep rows where point estimate is outside of bounds
  df_prob <- subset(datLong2, Deaths_Check == 1 | Fraction_Check == 1 | Rate_Check == 1)
  df_prob$Variable <- sub('\\_.*', '', df_prob$variable)
  df_prob$Quantile <-sub('.*\\_', '', df_prob$variable)
  df_prob$flag <- 0
  df_prob$flag[df_prob$Deaths_Check == 1 & df_prob$Variable == "Deaths"] <- 1
  df_prob$flag[df_prob$Fraction_Check == 1 & df_prob$Variable == "Fraction"] <- 1
  df_prob$flag[df_prob$Rate_Check == 1 & df_prob$Variable == "Rate"] <- 1
  df_prob <- data.frame(df_prob)[,c(idVarsAux, "cod", "Variable", "Quantile", "value", "flag")]
  if(!REGIONAL){
    df_prob <- df_prob[order(df_prob$ISO3, df_prob$Year, df_prob$cod, df_prob$Variable, df_prob$Quantile),]
  }else{
    df_prob <- df_prob[order(df_prob$Region, df_prob$Year, df_prob$cod, df_prob$Variable, df_prob$Quantile),]
  }
  
  if(nrow(df_prob) >= 1){
    warning("Point estimates are outside bounds")
  }
  
  return(df_prob)
}

fn_adjust_pointint <- function(POINTINT, KEY_COD, REGIONAL = FALSE){
  
  #' @title Adjust point estimates, lower, and upper bounds for consistency
  # 
  #' @description When cause-specific deaths point estimate is between 0 and 1, change LB of fractions/rates/deaths to 0.
  #' When cause-specific deaths point estimate = 0 and UB of cause-specific rate < cause-specific rate point estimate, change point estimate of rate to 0.
  #' When cause-specific deaths point estimate = 0 and UB of cause-specific fraction < cause-specific fraction point estimate, change point estimate of fraction to 0.
  #' When IGME all-cause deaths = 0, change cause-specific fractions/deaths/rates LB and point estimates to 0, change cause-specific UB for fractions/deaths to 1, change cause-specific UB for rates to same as all-cause UB, change cause-specific UB for fractions/rates/deaths Measles, Malaria, Natural Disasters, Collective Violence to 0.
  #' Adjust LB and UB of cause-specific fractions that fall on wrong side of point estimate.
  #' 2023.06.08 PATCH that manually adjusts some confidence intervals.
  #
  #' @param POINTINT Data frame with rounded point estimates, lower, and upper bounds for fractions/deaths/rates
  #' @param KEY_COD Data frame with age-specific CODs with different levels of classification.
  #' @param REGIONAL Boolean with true/false value if regional estimates.
  #' @return Data frame with rounded point estimates, lower, and upper bounds for fractions/deaths/rates that have been adjusted for inconsistencies.
  
  dat <- POINTINT
  # Causes of death for this age group
  # v_cod <- CODALL[CODALL %in% names(dat)]
  v_cod <- unique(KEY_COD$Reclass)  # Vector with ALL CAUSES OF DEATH (including single-cause estimates)
  v_cod <- v_cod[!v_cod %in% c("Other", "Undetermined")]
  v_other <- names(dat)[!(names(dat) %in% v_cod)]
  
  ## Adjustments to cause-specific fractions/rates/deaths due to cause-specific deaths between 0 and 1
  
  # Reshape to long
  datLong <- melt(setDT(dat), measure.vars = v_cod, variable.name = "cod")
  datLong$cod <- as.character(datLong$cod)
  
  if(REGIONAL){
    datLong$id <- paste0(datLong$Region, datLong$Year, datLong$Sex, datLong$cod)
  }else{
    datLong$id <- paste0(datLong$ISO3, datLong$Year, datLong$Sex, datLong$cod)
  }
  
  # When cause-specific deaths point estimate <= 1, change lower bound of fractions/rates/deaths to 0
  id_lbAllADJ <- unique(subset(datLong, Variable == "Deaths" & Quantile == "Point" & value <= 1)$id)
  datLong$value[datLong$id %in% id_lbAllADJ & datLong$Quantile == "Lower"] <- 0
  
  # Reshape to wide to identify more values in need of adjustment
  # All adjustments will still be made to datLong
  datWide <- dcast(setDT(datLong),  id ~ Variable+Quantile, value.var = "value")
  
  # When cause-specific deaths point estimate is between 0 and 1 and also larger than the upper bound, change the upper bound to 1
  # Note: this one commented out  in Pancho"s function AdjustUncert()
  #id_ubDeathsADJ <- unique(subset(datWide, Deaths_Point > 0 & Deaths_Point < 1 & Deaths_Point > Deaths_Upper)$id)
  #datLong$value[datLong$id %in% id_ubDeathsADJ & datLong$Quantile == "Upper" & datLong$Variable == "Deaths"] <- 1
  
  # When cause-specific deaths point estimate = 0 and upper bound of cause-specific rate < cause-specific rate point estimate, change point estimate of rate to 0.
  id_pointRateADJ <- unique(subset(datWide, Deaths_Point == 0 & Rate_Upper < Rate_Point)$id)
  datLong$value[datLong$id %in% id_pointRateADJ & datLong$Quantile == "Point" & datLong$Variable == "Rate"] <- 0
  
  # When cause-specific deaths point estimate = 0 and upper bound of cause-specific fraction < cause-specific fraction point estimate, change point estimate of fraction to 0.
  id_pointFracADJ <- unique(subset(datWide, Deaths_Point == 0 & Fraction_Upper < Fraction_Point)$id)
  datLong$value[datLong$id %in% id_pointFracADJ & datLong$Quantile == "Point" & datLong$Variable == "Fraction"] <- 0
  
  ## Adjustments to cause-specific fractions/rates/deaths due to zero IGME all-cause deaths
  
  if(!REGIONAL){
    
    # Country-years with 0 IGME deaths
    id_noIGMEDeaths <- unique(subset(datLong, Deaths2 == 0 & Quantile == "Point")$id)
    
    # Change lower bound and point estimate to 0.
    datLong$value[datLong$id %in% id_noIGMEDeaths & datLong$Quantile == "Lower"] <- 0
    datLong$value[datLong$id %in% id_noIGMEDeaths & datLong$Quantile == "Point"] <- 0
    
    # Change cause-specific upper bounds for fractions and deaths to 1.
    datLong$value[datLong$id %in% id_noIGMEDeaths & datLong$Quantile == "Upper"& datLong$Variable %in% c("Fraction", "Deaths")] <- 1
    
    # Change cause-specific upper bounds for rates to the same as the all-cause upper bound.
    datLong$value[datLong$id %in% id_noIGMEDeaths & datLong$Quantile == "Upper" & datLong$Variable == "Rate"] <- 
      datLong$Rate[datLong$id %in% id_noIGMEDeaths & datLong$Quantile == "Upper" & datLong$Variable == "Rate"]
    
    # Change certain cause-specific upper bounds for fractions/rates/deaths to 0.
    # Measles
    if("Measles" %in% v_cod){
      datLong$value[datLong$id %in% id_noIGMEDeaths & datLong$Quantile == "Upper" & datLong$cod == "Measles"] <- 0  
    }
    # Malaria
    if("Malaria" %in% v_cod){
      datLong$value[datLong$id %in% id_noIGMEDeaths & datLong$Quantile == "Upper" & datLong$cod == "Malaria"] <- 0  
    }
    # Natural disasters
    if("NatDis" %in% v_cod){
      datLong$value[datLong$id %in% id_noIGMEDeaths & datLong$Quantile == "Upper" & datLong$cod == "NatDis"] <- 0  
    }
    # Collective violence
    if("CollectVio" %in% v_cod){
      datLong$value[datLong$id %in% id_noIGMEDeaths & datLong$Quantile == "Upper" & datLong$cod == "CollectVio"] <- 0  
    }
  }
  
  # Reshape to wide to identify more values in need of adjustment
  # All adjustments will still be made to datLong
  datWide <- dcast(setDT(datLong),  id ~ Variable+Quantile, value.var = "value")
  
  ## Adjustments to lower/upper bounds of cause-specific fractions that fall on wrong side of point estimate
  id_lowerFracADJ <- unique(subset(datWide, Fraction_Lower > Fraction_Point)$id)
  datLong$value[datLong$id %in% id_lowerFracADJ & datLong$Quantile == "Lower" & datLong$Variable == "Fraction"] <- 
    datLong$value[datLong$id %in% id_lowerFracADJ & datLong$Quantile == "Lower" & datLong$Variable == "Deaths"]/
    datLong$Deaths2[datLong$id %in% id_lowerFracADJ & datLong$Quantile == "Point" & datLong$Variable == "Deaths"]
  
  id_upperFracADJ <- unique(subset(datWide, Fraction_Upper < Fraction_Point)$id)
  datLong$value[datLong$id %in% id_upperFracADJ & datLong$Quantile == "Upper" & datLong$Variable == "Fraction"] <- 
    datLong$value[datLong$id %in% id_upperFracADJ & datLong$Quantile == "Upper" & datLong$Variable == "Deaths"]/
    datLong$Deaths2[datLong$id %in% id_upperFracADJ & datLong$Quantile == "Point" & datLong$Variable == "Deaths"]
  
  # Reshape datLong to original form
  df_res <- datLong
  df_res$id <- NULL
  df_res <- dcast(df_res, ... ~ cod, value.var = "value")
  df_res <- data.frame(df_res)[, c(v_other, v_cod)]
  if(!REGIONAL){
    df_res <- df_res[order(df_res$ISO3, df_res$Year, df_res$Sex, df_res$Variable, df_res$Quantile),]
  }else{
    df_res <- df_res[order(df_res$Region, df_res$Year, df_res$Sex, df_res$Variable, df_res$Quantile),]
  }
  rownames(df_res) <- NULL
  
  if(!REGIONAL){
    #------------------------#
    # 2023.06.08 PATCH
    # For rates, multiply by 100, floor() to remove decimals, divide by 100
    # if(ageGroup == "05to09"){
    #   # PANCHO's adjustments commented out
    #   # Measles in Turkey
    #   df_res$Measles[which(df_res$ISO3 == "TUR" & df_res$Year %in% c(2003, 2008, 2009) &
    #                       df_res$Variable == "Rate" & df_res$Quantile == "Upper")] <-
    #     df_res$Measles[which(df_res$ISO3 == "TUR" & df_res$Year %in% c(2003, 2008, 2009) &
    #                         df_res$Variable == "Rate" & df_res$Quantile == "Upper")] + 0.00002
    #   # HIV in Bangladesh
    #   df_res$HIV[which(df_res$ISO3 == "BGD" & df_res$Year %in% c(2007) &
    #                   df_res$Variable == "Rate" & df_res$Quantile == "Upper")] <-
    #     df_res$HIV[which(df_res$ISO3 == "BGD" & df_res$Year %in% c(2007) &
    #                     df_res$Variable == "Rate" & df_res$Quantile == "Upper")] + 0.00003
    # }
    if(ageGroup == "10to14"){
      # Somalia Nat Disasters in 2011
      # df_res$NatDis[which(df_res$ISO3 == "SOM" & df_res$Year == 2011 &
      #                    df_res$Variable == "Rate" & df_res$Quantile == "Lower")] <-
      #   floor(100*df_res$NatDis[which(df_res$ISO3 == "SOM" & df_res$Year == 2011 &
      #                       df_res$Variable == "Rate" & df_res$Quantile == "Lower")]) / 100
      # Micronesia (FSM) OtherCMPN in 2002
      df_res$OtherCMPN[which(df_res$ISO3 == "FSM" & df_res$Year == 2002 &
                               df_res$Variable == "Deaths" & df_res$Quantile == "Upper")] <-
        df_res$OtherCMPN[which(df_res$ISO3 == "FSM" & df_res$Year == 2002 &
                                 df_res$Variable == "Deaths" & df_res$Quantile == "Upper")] + 1
      # Somalia HIV in 2010
      df_res$HIV[which(df_res$ISO3 == "SOM" & df_res$Year == 2010 &
                         df_res$Variable == "Rate" & df_res$Quantile == "Lower")] <-
        floor(100*df_res$HIV[which(df_res$ISO3 == "SOM" & df_res$Year == 2010 &
                                     df_res$Variable == "Rate" & df_res$Quantile == "Lower")]) / 100
    }
    if(ageGroup == "15to19f"){
      # Ukrainian females CollectVio in 2014 (both Pancho's and mine)
      df_res[which(df_res$ISO3 == "UKR" & df_res$Year == 2014 &
                     df_res$Variable == "Fraction" & df_res$Quantile == "Upper"), "CollectVio"] <-
        df_res[which(df_res$ISO3 == "UKR" & df_res$Year == 2014 &
                       df_res$Variable == "Fraction" & df_res$Quantile == "Upper"), "CollectVio"] + 0.0001
      # Jamaica females HIV in 2012
      df_res$HIV[which(df_res$ISO3 == "JAM" & df_res$Year == 2012 &
                         df_res$Variable == "Rate" & df_res$Quantile == "Upper")] <-
        df_res$HIV[which(df_res$ISO3 == "JAM" & df_res$Year == 2012 &
                           df_res$Variable == "Rate" & df_res$Quantile == "Upper")] + 0.0001
      
    }
    if(ageGroup == "15to19m"){
      #   # Syrian males 2014
      #   df_res[which(df_res$ISO3 == "SYR" & df_res$Year == 2014 &
      #               df_res$Variable == "Rate" & df_res$Quantile == "Upper"), "InterpVio"] <-
      #     df_res[which(df_res$ISO3 == "SYR" & df_res$Year == 2014 &
      #                 df_res$Variable == "Rate" & df_res$Quantile == "Upper"), "InterpVio"] + .05
      #   
      #   # Syrian males 2019
      #   df_res[which(df_res$ISO3 == "SYR" & df_res$Year == 2019 &
      #               df_res$Variable == "Rate" & df_res$Quantile == "Upper"), c("OtherNCD", "RTI", "OtherInj")] <-
      #     df_res[which(df_res$ISO3 == "SYR" & df_res$Year == 2019 &
      #                 df_res$Variable == "Rate" & df_res$Quantile == "Upper"), c("OtherNCD", "RTI", "OtherInj")] + 0.005
      #   
      #   # Somoan males 2009
      #   df_res[which(df_res$ISO3 == "WSM" & df_res$Year == 2009 & 
      #               df_res$Variable == "Rate" & df_res$Quantile == "Upper"), "NatDis"] <-
      #     df_res[which(df_res$ISO3 == "WSM" & df_res$Year == 2009 &
      #                 df_res$Variable == "Rate" & df_res$Quantile == "Upper"), "NatDis"] + .1
      #   # Tajikistan males 2000
      #   df_res[which(df_res$ISO3 == "TJK" & df_res$Year == 2000 &
      #               df_res$Variable == "Rate" & df_res$Quantile == "Upper"), "CollectVio"] <-
      #     df_res[which(df_res$ISO3 == "TJK" & df_res$Year == 2000 &
      #                 df_res$Variable == "Rate" & df_res$Quantile == "Upper"), "CollectVio"] + 0.003
      # Trinidad and Tobago males HIV 2008 and 2009
      df_res[which(df_res$ISO3 == "TTO" & df_res$Year == 2008 &
                     df_res$Variable == "Rate" & df_res$Quantile == "Upper"), "HIV"] <-
        df_res[which(df_res$ISO3 == "TTO" & df_res$Year == 2008 &
                       df_res$Variable == "Rate" & df_res$Quantile == "Upper"), "HIV"] + 0.0005
      df_res[which(df_res$ISO3 == "TTO" & df_res$Year == 2009 &
                     df_res$Variable == "Rate" & df_res$Quantile == "Upper"), "HIV"] <-
        df_res[which(df_res$ISO3 == "TTO" & df_res$Year == 2009 &
                       df_res$Variable == "Rate" & df_res$Quantile == "Upper"), "HIV"] + 0.0005
    }
    # END PATCH
    #------------------------#
  }
  
  return(df_res)
  
}
