
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

fn_rearrange_draws <- function(L_DRAWS){
  
  #' @title Rearrange draws of predicted fractions
  # 
  #' @description Transforms nested list (first level = year, second level = draws) into a list 
  #' where each element is a draw which includes all predicted CSMFs for that year.
  #
  #' @param L_DRAWS List of length number of years being predicted.
  #' Each first-level list element is a list of length number of draws.
  #' Within first-level list element, each second-level list element is a data frame with predicted CSMFs for one year.
  #' @return List of length number of draws.
  #' Each list element is a data frame with predicted CSMFs for one draw.
  
  # Add sequential name for draws
  # Transform list of all draws into a data frame with an index column for "draw"
  l_draws <- lapply(L_DRAWS, function(x){ names(x) <- 1:length(x)
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

fn_create_sample_vectors <- function(L_DRAWS_HMM, L_DRAWS_LMM, L_DRAWS_ENV){
  
  #' @title Create randomly sampled vectors for igme draws and predicted fractions
  # 
  #' @description Randomly samples draws from envelopes and predicted LMM and HMM fractions.
  #' Saves sample vectors in a list.
  #
  #' @param L_DRAWS_HMM List of length number of draws.
  #' Each list element is a data frame with CSMFs for all years being predicted for HMM countries.
  #' @param L_DRAWS_LMM List of length number of draws.
  #' Each list element is a data frame with CSMFs for all years being predicted for LMM countries.
  #' @param L_DRAWS_ENV List of length three, corresponding to crisis-free deaths, crisis-included deaths, crisis-included rates.
  #' Each first-level list element is a list of length number of draws.
  #' Within first-level list element, each second-level list element is a data frame 
  #' with a draw with columns c("ISO3", "Year", "Sex", "Deaths1", "Deaths2", "Rate").
  #' @return List of length three, corresponding to envelope, HMM, LMM.
  #' Each list element is a vector (all of equal length) with a list of integers.
  #' Integers are randomly sampled draws from envelope, HMM, LMM.
  
  if(length(L_DRAWS_HMM) >= length(L_DRAWS_LMM)){
    # Sample from IGME draws, number of samples = number of draws for predicted fractions
    #v_sample_env <- sort(sample(x = dim(L_DRAWS_ENV$deaths)[3], size = length(L_DRAWS_HMM))) 
    v_sample_env <- sort(sample(x = length(L_DRAWS_ENV$deaths1), size = length(L_DRAWS_HMM))) 
    
    # Sets of HMM fractions
    v_sample_HMM <- 1:length(L_DRAWS_HMM)
    
    # Sets of LMM fractions
    # With extra samples if there are fewer LMM draws than HMM
    v_sample_LMM <- c(1:length(L_DRAWS_LMM),
                      sort(sample(x = 1:length(L_DRAWS_LMM),
                                  size = length(L_DRAWS_HMM) - length(L_DRAWS_LMM), replace = T)))
  }else{
    
    # Sample from IGME draws, number of samples = number of draws for predicted fractions
    #v_sample_env <- sort(sample(x = dim(L_DRAWS_ENV$deaths)[3], size = length(L_DRAWS_LMM)))
    v_sample_env <- sort(sample(x = length(L_DRAWS_ENV$deaths1), size = length(L_DRAWS_LMM))) 
    
    # Sets of HMM fractions
    # With extra samples if there are fewer HMM draws than LMM
    v_sample_HMM <- c(1:length(L_DRAWS_HMM),
                      sort(sample(x = 1:length(L_DRAWS_HMM),
                                  size = length(L_DRAWS_LMM) - length(L_DRAWS_HMM), replace = T)))
    
    # Sets of LMM fractions
    v_sample_LMM <- 1:length(L_DRAWS_LMM)
  }
  
  # Combine all sample vectors into list
  v_sample <- list(v_sample_env, v_sample_HMM, v_sample_LMM)
  names(v_sample) <- c("env", "HMM", "LMM")
  
  return(v_sample)
}

fn_rand_draw_env <- function(L_DRAWS_ENV, V_SAMPLE){
  
  #' @title Random draw from IGME envelopes with sample vector
  # 
  #' @description Indexes randomly sampled draws from envelope. Merges crisis-free/included draws together.
  #' 
  #' @param L_DRAWS_ENV List of length three, corresponding to crisis-free deaths, crisis-included deaths, crisis-included rates.
  #' Each first-level list element is a list of length number of draws.
  #' Within first-level list element, each second-level list element is one envelope draw:
  #' a data frame with columns c("ISO3", "Year", "Sex", "Deaths1", "Deaths2", "Rate").
  #' @param V_SAMPLE List of length three, corresponding to envelope, HMM, LMM.
  #' Each list element is a vector (all of equal length) with a list of integers.
  #' Integers are randomly sampled draws from envelope, HMM, LMM.
  #' @return List of length that is the same as number of V_SAMPLE integers.
  #' Each list element is one envelope draw: a data frame with columns c("ISO3", "Year", "Sex", "Deaths1", "Deaths2", "Rate").
  
  # Crisis-free IGME deaths (random draw)
  l_deaths1 <- lapply(V_SAMPLE, function(x){ L_DRAWS_ENV$deaths1[[x]] })
  
  # Crisis-included IGME deaths (random draw)
  l_deaths2 <- lapply(V_SAMPLE, function(x){ L_DRAWS_ENV$deaths2[[x]] })
  
  # Crisis-included IGME rates (random draw)
  l_rates2 <- lapply(V_SAMPLE, function(x){ L_DRAWS_ENV$rates2[[x]] })
  
  # Merge
  l_sampled_draws <- mapply(function(x, y) merge(x, y, by = idVars, all=TRUE), x = l_deaths1, y = l_deaths2, SIMPLIFY = FALSE)
  l_sampled_draws <- mapply(function(x, y) merge(x, y, by = idVars, all=TRUE), x = l_sampled_draws, y = l_rates2, SIMPLIFY = FALSE)
  
  # Tidy up
  l_sampled_draws <- lapply(l_sampled_draws, function(x){ x[order(x$ISO3, x$Year),] })
  l_sampled_draws <- lapply(l_sampled_draws, function(x){ x[, c(idVars, sort(names(x)[!(names(x) %in% idVars)]))] })
  
  return(l_sampled_draws)
}

fn_format_draws <- function(L_DAT_HMM, L_DAT_LMM){
  
  #' @title Format draws of predicted fractions
  # 
  #' @description Combines randomly sampled draws for HMM and LMM.
  #' 
  #' @param L_DRAWS_HMM List of length number of draws.
  #' Each list element is a data frame with CSMFs for all years being predicted for HMM countries.
  #' @param L_DRAWS_LMM List of length number of draws.
  #' Each list element is a data frame with CSMFs for all years being predicted for LMM countries.
  #' @return List of length number of draws.
  #' Each list element is a data frame with predicted CSMFs for HMM and LMM for one draw.
  
  l_dat <- mapply(rbind, L_DAT_HMM, L_DAT_LMM, SIMPLIFY=FALSE)

  # Rearrange columns
  l_dat <- lapply(l_dat, function(x){ x[, c(idVars, sort(names(x)[which(!names(x) %in% idVars)]))] })

  # Tidy up
  l_dat <- lapply(l_dat, function(x){ x[order(x$ISO3, x$Year),] })
  
  return(l_dat)
  
}

fn_rand_assign_vr <- function(DAT, DRAW_ENV, KEY_COD, CTRYGRP){
  
  #' @title Randomly assign CSMF values for goodvr/China for current draw
  # 
  #' @description Sample from multinomial distribution to perturb CSMFs for goodVR/China for current draw.
  #' 
  #' @param DAT Data frame with CSMFs for goodvr/China.
  #' @param DRAW_ENV Data frame that is one envelope draw with columns c("ISO3", "Year", "Sex", "Deaths1", "Deaths2", "Rate").
  #' @param KEY_COD Data frame with age-specific CODs with different levels of classification.
  #' @param CTRYGRP Character string that must be set as either "GOODVR" or "CHN".
  #' @return Data frame with randomly sampled CSMFs.
  
  if(!(CTRYGRP %in% c("GOODVR", "CHN"))){
    stop("Must set CTRYGRP as either GOODVR or CHN")
  }

  # Vector with all causes of death (including single-cause estimates)
  v_cod <- unique(KEY_COD$Reclass)  
  v_cod <- v_cod[!v_cod %in% c("Other", "Undetermined")]
  # If China, also exclude HIV as this will be added through squeezing
  if(CTRYGRP == "CHN"){ 
    v_cod <- v_cod[v_cod != "HIV"]
  }
  
  # Incorporate IGME deaths and rates (Random draw)
  draw_dat <- merge(DAT[, !names(DAT) %in% c("Deaths", "Rate")],
                    DRAW_ENV[, names(DRAW_ENV) != "Deaths1"], by = idVars, all.x = T)
  
  # Random CAUSE-SPECIFIC deaths from multinomial distribution
  draw_dat[, paste(v_cod)] <- t(apply(draw_dat[, c(v_cod, "Deaths2")], 1,
                                  function(x) {
                                    rmultinom(n = 1, size = round(x["Deaths2"]),
                                              prob = x[paste(v_cod)])
                                  }))
  
  # Transform into fractions
  draw_dat[, paste(v_cod)] <- draw_dat[, paste(v_cod)] / rowSums(draw_dat[, paste(v_cod)])
  
  # Re-label variables
  names(draw_dat)[names(draw_dat) == "Deaths2"] <- "Deaths"
  
  # For China, delete envelope columns
  # Envelopes will be merged on during squeezing
  if(CTRYGRP == "CHN"){
    draw_dat <- draw_dat[, !names(draw_dat) %in% c("Deaths", "Rate")]
  }
  
  # Return random draw for GOODVR or CHN
  return(draw_dat)

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

fn_rand_assign_meas <- function(DAT){

  #' @title Randomly assign measles deaths for current draw
  # 
  #' @description Cap total measles deaths (endemic + epidemic) to crisis-included envelope.
  #' If total measles deaths > 0, sample new total from log normal distribution.
  #' If endemic measles deaths > 0, recalculate endemic by subtracting original epidemic measles from randomly sampled total.
  #' If epidemic measles deaths > 0, recalculate epidemic by subtracting original endemic measles from randomly sampled total.
  #' 
  #' @param DAT Data frame with predicted CSMFs, single cause data, envelopes, and minimum fractions.
  #' @return Data frame with randomly sampled measles deaths.
  
  # Cap the upper bound of Measles (EPI + END) to the envelope
  DAT$msl_ub[which(DAT$msl_ub > DAT$Deaths2)] <- DAT$Deaths2[which(DAT$msl_ub > DAT$Deaths2)]
  
  # Uncertainty intervals only available for endemic + epidemic Measles
  # Random epidemic + endemic Measles
  
  # Randomly sample value for total measles
  v_msl <- rep(0, nrow(DAT))
  v_idSqz <- which(DAT$msl != 0)
  if(length(v_idSqz) > 0){
    # Sample from log normal distribution
    v_msl[v_idSqz] <- fn_sample_log_norm(MU = DAT$msl[v_idSqz],
                                         LB = DAT$msl_lb[v_idSqz],
                                         UB = DAT$msl_ub[v_idSqz])
    # Save point estimate for endemic measles
    v_meas_end <- DAT$Measles
    v_idSqz <- which(DAT$Measles != 0)
    # Calculate random endemic measles by subtracting epidemic from random total
    DAT$Measles[v_idSqz] <- v_msl[v_idSqz] - DAT$meas_epi[v_idSqz] 
    # Avoid negative values
    DAT$Measles[which(DAT$Measles < 0)] <- 0
    # Calculate random epidemic measles by subtracting endemic point estimate from random total
    v_idSqz <- which(DAT$meas_epi != 0)
    DAT$meas_epi[v_idSqz] <- v_msl[v_idSqz] - v_meas_end[v_idSqz]
    # Note: Measles and meas_epi do not need to add up to msl, but their total should be within msl_lb and msl_ub
    ## meas_epi will be added on top of envelope
  }
  
  return(DAT)
  
}

fn_rand_assign_tb <- function(DAT){
  
  #' @title Randomly assign TB deaths for current draw
  # 
  #' @description Cap TB deaths (TB and TBre) to crisis-included envelope.
  #' If TB deaths != 0 and is within lower and upper TB bounds, sample from log normal distribution.
  #' 
  #' @param DAT Data frame with predicted CSMFs, single cause data, envelopes, and minimum fractions.
  #' @return Data frame with randomly sampled TB deaths.
  
  # Cap the upper bound of TB to the envelope
  DAT$tb_ub[which(DAT$tb_ub > DAT$Deaths1)] <- DAT$Deaths1[which(DAT$tb_ub > DAT$Deaths1)]
  # Values to be squeezed
  v_idSqz <- which(DAT$TB != 0 & DAT$TB > DAT$tb_lb & DAT$TB < DAT$tb_ub)
  # Sample random values
  if (length(v_idSqz) > 0) {
    DAT$TB[v_idSqz] <- fn_sample_log_norm(MU = DAT$TB[v_idSqz],
                                          LB = DAT$tb_lb[v_idSqz],
                                          UB = DAT$tb_ub[v_idSqz])
  }
  
  if(respTB){
    
    # Cap the upper bound of TB to the envelope
    DAT$tbre_ub[which(DAT$tbre_ub > DAT$Deaths1)] <- DAT$Deaths1[which(DAT$tbre_ub > DAT$Deaths1)]
    # Values to be squeezed
    v_idSqz <- which(DAT$TBre != 0 & DAT$TBre > DAT$tbre_lb & DAT$TBre < DAT$tbre_ub)
    # Sample random values
    if (length(v_idSqz) > 0) {
      DAT$TBre[v_idSqz] <- fn_sample_log_norm(MU = DAT$TBre[v_idSqz],
                                              LB = DAT$tbre_lb[v_idSqz],
                                              UB = DAT$tbre_ub[v_idSqz])
    }
  }
  
  return(DAT)
}

fn_rand_assign_hiv <- function(DAT){
  
  #' @title Randomly assign HIV deaths for current draw
  # 
  #' @description If HIV deaths != 0 and is within lower and upper HIV bounds, sample from log normal distribution.
  #' 
  #' @param DAT Data frame with predicted CSMFs, single cause data, envelopes, and minimum fractions.
  #' @return Data frame with randomly sampled HIV deaths.
  
  # Values to be squeezed
  v_idSqz <- which(DAT$HIV != 0 & DAT$HIV > DAT$hiv_lb & DAT$HIV < DAT$hiv_ub)
  
  # Sample random values
  if (length(v_idSqz) > 0) {
    DAT$HIV[v_idSqz] <- rtnorm(n = length(v_idSqz), mean = DAT$HIV[v_idSqz],
                               sd = (DAT$hiv_ub[v_idSqz] - DAT$hiv_lb[v_idSqz]) / 3.92,
                               lower = 0)
  }
  
  return(DAT)
}

fn_calc_ui <- function(DRAWS_CSMF, UI, CODALL){
  
  #' @title Calculate uncertainty intervals from draws
  # 
  #' @description Create a separate list for CSMFs, rates, and deaths.
  #' Convert each list of data frames to an array.
  #' Calculate the quantile for each cell across the matrices of the array.
  #' Save matrices with quantiles as data frames with identifying columns.
  #' Combine all data frames.
  #
  #' @param DRAWS_CSMF List of length number of draws of predicted fractions.
  #' Each list element is a data frame with CSMFs for every country-year for all CODs estimated.
  #' Also contains columns c("ISO3", "Year", "Sex", "Deaths", "Rate")
  #' @param UI  Integer with the width of the uncertainty interval desired.
  #' @param CODALL  
  #' @return Data frame with lower and upper quantiles for each COD for deaths, fractions, rates.
  
  # Create interval
  UI <- 1/2 + c(- UI, UI) / 2
  
  # Causes of death for this age group
  v_cod <- CODALL[CODALL %in% names(DRAWS_CSMF[[1]])]
  
  # Data frame with identifying columns
  df_idcols <- DRAWS_CSMF[[1]][, !names(DRAWS_CSMF[[1]]) %in% c('Deaths', 'Rate', paste(v_cod))]
  
  # Create lists for fractions, rates, deaths
  l_frac <- DRAWS_CSMF
  l_rates <- lapply(DRAWS_CSMF, function(x){ x[,v_cod] * x[,"Rate"] })
  l_deaths <- lapply(DRAWS_CSMF, function(x){ x[,v_cod] * x[,"Deaths"] })
  
  # Convert each data.frame in list to matrix that includes only CODs
  l_frac <- lapply(l_frac, function(x) as.matrix(x[,v_cod]))
  l_rates <- lapply(l_rates, function(x) as.matrix(x[,v_cod]))
  l_deaths <- lapply(l_deaths, function(x) as.matrix(x[,v_cod]))
  
  # Convert list to array
  # Calculate quantiles for each cell across matrices of the array
  m_frac_lb <- apply(simplify2array(l_frac), c(1,2), quantile, UI[1], na.rm = T)
  m_frac_ub <- apply(simplify2array(l_frac), c(1,2), quantile, UI[2], na.rm = T)
  m_rates_lb <- apply(simplify2array(l_rates), c(1,2), quantile, UI[1], na.rm = T)
  m_rates_ub <- apply(simplify2array(l_rates), c(1,2), quantile, UI[2], na.rm = T)
  m_deaths_lb <- apply(simplify2array(l_deaths), c(1,2), quantile, UI[1], na.rm = T)
  m_deaths_ub <- apply(simplify2array(l_deaths), c(1,2), quantile, UI[2], na.rm = T)
  
  # Format arrays in to data frames
  df_frac_lb <- as.data.frame(cbind(df_idcols, 
                                    Variable = rep('Fraction', nrow(df_idcols)),
                                    Quantile = rep('Lower', nrow(df_idcols)), 
                                    m_frac_lb))
  df_frac_ub <- as.data.frame(cbind(df_idcols, 
                                    Variable = rep('Fraction', nrow(df_idcols)),
                                    Quantile = rep('Upper', nrow(df_idcols)), 
                                    m_frac_ub))
  df_rates_lb <- as.data.frame(cbind(df_idcols, 
                                     Variable = rep('Rate', nrow(df_idcols)),
                                     Quantile = rep('Lower', nrow(df_idcols)), 
                                     m_rates_lb))
  df_rates_ub <- as.data.frame(cbind(df_idcols, 
                                     Variable = rep('Rate', nrow(df_idcols)),
                                     Quantile = rep('Upper', nrow(df_idcols)), 
                                     m_rates_ub))
  df_deaths_lb <- as.data.frame(cbind(df_idcols, 
                                      Variable = rep('Deaths', nrow(df_idcols)),
                                      Quantile = rep('Lower', nrow(df_idcols)), 
                                      m_deaths_lb))
  df_deaths_ub <- as.data.frame(cbind(df_idcols, 
                                      Variable = rep('Deaths', nrow(df_idcols)),
                                      Quantile = rep('Upper', nrow(df_idcols)), 
                                      m_deaths_ub))  
  # Combine and tidy
  df_res <- rbind(df_frac_lb, df_frac_ub, df_rates_lb, df_rates_ub, df_deaths_lb, df_deaths_ub)
  df_res <- df_res[order(df_res$ISO3, df_res$Year, df_res$Sex, df_res$Variable, df_res$Quantile),]
  rownames(df_res) <- NULL
  
  return(df_res)
}



##################################################
####
####   Calculate uncertainty intervals from draws
####
##################################################

# NOTE: make this usable on regions
# make different function for AARR

#test <- draws_csmf_ALL[1:5]
#UI = .95
#res <- fn_calc_ui(test)

#DRAWS_CSMF <- draws_csmf_ALL[1:5]
#UI <- .95

