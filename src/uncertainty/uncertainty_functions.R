
##################################################
####
####   Rearrange draws of predicted fractions
####
##################################################

fn_rearrange_draws <- function(L_DRAWS){

  # Transform from a list of lists (first level = year, second level = draws)
  # Into a list where each element is a year containing all draws for that year
  L_DRAWS <- lapply(L_DRAWS, function(x){ names(x) <- 1:length(x)
                                                  x <- ldply(x, .id = "draw")
                                                  return(x)})
  df_draws <- ldply(L_DRAWS)
  
  # Split into list by "draw" column
  l_draws <- split(df_draws, df_draws$draw)
  l_draws <- lapply(l_draws, function(x){ x <- x[order(x$ISO3, x$Year),]})
  
  # Tidy up
  l_draws <- lapply(l_draws, function(x){x$draw <- NULL ; return(x)})
  l_draws <- lapply(l_draws, function(x){ x[, c(idVars, sort(names(x)[which(!names(x) %in% idVars)]))] ; return(x)})
  l_draws <- lapply(l_draws, function(x){ rownames(x) <- NULL ; return(x)})
  
  return(l_draws)
}

##################################################
####
####   Format draws of predicted fractions
####
##################################################

#L_DAT_HMM <- draws_Rearranged_HMM
#L_DAT_LMM <- draws_Rearranged_LMM

fn_format_draws <- function(L_DAT_HMM, L_DAT_LMM){
  
  l_dat <- mapply(rbind, L_DAT_HMM, L_DAT_LMM, SIMPLIFY=FALSE)

  # Rearrange columns
  l_dat <- lapply(l_dat, function(x){ x[, c(idVars, sort(names(x)[which(!names(x) %in% idVars)]))] })

  # Tidy up
  l_dat <- lapply(l_dat, function(x){ x[order(x$ISO3, x$Year),] })
  
  return(l_dat)
  
}


######## Combining HMM and LMM

# L_DRAWS_HMM <- l_draws_HMM
# L_DRAWS_LMM <- l_draws_LMM
# 
# # Transform from a list of lists (first level = year, second level = draws)
# # Into a list where each element is a year containing all draws for that year
# L_DRAWS_HMM <- lapply(L_DRAWS_HMM, function(x){ names(x) <- 1:length(x)
#                                                 x <- ldply(x, .id = "draw")
#                                                 return(x)})
# df_draws_HMM <- ldply(L_DRAWS_HMM)
# L_DRAWS_LMM <- lapply(L_DRAWS_LMM, function(x){ names(x) <- 1:length(x)
#                                                 x <- ldply(x, .id = "draw")
#                                                 return(x)})
# df_draws_LMM <- ldply(L_DRAWS_LMM)
# 
# # Combine LMM and HMM
# df_draws <- rbind(df_draws_LMM, df_draws_HMM)
# # Split into list by "draw" column
# l_draws <- split(df_draws, df_draws$draw)
# l_draws <- lapply(l_draws, function(x){ x <- x[order(x$ISO3, x$Year),]})
# 
# # Tidy up
# l_draws <- lapply(l_draws, function(x){x$draw <- NULL ; return(x)})
# l_draws <- lapply(l_draws, function(x){ x[, c(idVars, sort(names(x)[which(!names(x) %in% idVars)]))] ; return(x)})
# l_draws <- lapply(l_draws, function(x){ rownames(x) <- NULL ; return(x)})


### old ###

# First level of list is year. there are 22 elements.
# Within each year, there are ~1000 draws
# Need to combine all the first draws from each element, etc.
# df_test <- lapply(l_test, function(x){ x <- ldply(x); return(x)})
# 
# fn_format_predicted_draws <- function(L_DRAWS_HMM, L_DRAWS_LMM){
#   
#   
#   mapply(function(X,Y) {
#     sapply(1:10, function(row) cor(X[row,], Y[row,]))
#   }, X=L_DRAWS_HMM, L_DRAWS_LMM)
#   
#   L_DRAWS <- mapply(cbind, L_DRAWS_HMM, L_DRAWS_HMM, SIMPLIFY=F)
#   
#   
#   
#   
#   
#   datHMM <- do.call(rbind, L_DAT_HMM)
#   datLMM <- do.call(rbind, L_DAT_LMM)
#   dat <- rbind(datHMM, datLMM)
#   
#   # Rearrange columns
#   dat <- dat[, c(idVars, sort(names(dat)[which(!names(dat) %in% idVars)]))] 
#   
#   # Tidy up
#   dat <- dat[order(dat$ISO3, dat$Year),]
#   
#   return(dat)
#   
# }



##################################################
####
####   Nested lapply
####
##################################################


fn_nested_lapply <- function(data, fun) { lapply(data, function(sublist) { lapply(sublist, fun) })  }


##################################################
####
####  Create randomly sampled vectors for igme draws and predicted fractions
####
##################################################

#DRAWS_HMM <- draws_Rearranged_HMM
#DRAWS_LMM <- draws_Rearranged_LMM
#DRAWS_IGME <- draws_env

fn_create_sample_vectors <- function(DRAWS_HMM, DRAWS_LMM, DRAWS_ENV){
  
  if(length(DRAWS_HMM) >= length(DRAWS_LMM)){
    # Sample from IGME draws, number of samples = number of draws for predicted fractions
    v_sample_env <- sort(sample(x = dim(DRAWS_ENV$deaths)[3], size = length(DRAWS_HMM))) 
    
    # Sets of HMM fractions
    v_sample_HMM <- 1:length(DRAWS_HMM)
    
    # Sets of LMM fractions
    # With extra samples if there are fewer LMM draws than HMM
    v_sample_LMM <- c(1:length(DRAWS_LMM),
                    sort(sample(x = 1:length(DRAWS_LMM),
                                size = length(DRAWS_HMM) - length(DRAWS_LMM), replace = T)))
  }else{
    
    # Sample from IGME draws, number of samples = number of draws for predicted fractions
    v_sample_env <- sort(sample(x = dim(DRAWS_ENV$deaths)[3], size = length(DRAWS_LMM))) 
    
    # Sets of HMM fractions
    # With extra samples if there are fewer HMM draws than LMM
    v_sample_HMM <- c(1:length(DRAWS_HMM),
                    sort(sample(x = 1:length(DRAWS_HMM),
                                size = length(DRAWS_LMM) - length(DRAWS_HMM), replace = T)))
    
    # Sets of LMM fractions
    v_sample_LMM <- 1:length(DRAWS_LMM)
  }
  
  # Combine all sample vectors into list
  v_sample <- list(v_sample_env, v_sample_HMM, v_sample_LMM)
  names(v_sample) <- c("env", "HMM", "LMM")
  
  return(v_sample)
}


##################################################
####
####  Random draw from igme
####
##################################################

#DRAWS_ENV <- draws_env
#V_SAMPLE <- v_sample$env

fn_rand_draw_env <- function(DRAWS_ENV, V_SAMPLE){

  # Crisis-free IGME deaths (random draw)
  l_deaths1 <- lapply(V_SAMPLE, function(x) as.data.frame(DRAWS_ENV$deaths[, , x]))
  l_deaths1 <- lapply(l_deaths1, function(x){ x$ISO3 <- rownames(x) ; return(x)})
  l_deaths1 <- lapply(l_deaths1, function(x){ x <- as.data.frame(pivot_longer(x, cols = 1:(ncol(x)-1),
                                                     names_to = 'Year', values_to = 'Deaths1')) ; return(x)})
  l_deaths1 <- lapply(l_deaths1, function(x){ x$Sex <- sexLabel ; return(x)})
  
  # All-cause IGME deaths (random draw)
  l_deaths2 <- lapply(V_SAMPLE, function(x) as.data.frame(DRAWS_ENV$deathsAll[, , x]))
  l_deaths2 <- lapply(l_deaths2, function(x){ x$ISO3 <- rownames(x) ; return(x)})
  l_deaths2 <- lapply(l_deaths2, function(x){ x <- as.data.frame(pivot_longer(x, cols = 1:(ncol(x)-1),
                                                                              names_to = 'Year', values_to = 'Deaths2')) ; return(x)})
  l_deaths2 <- lapply(l_deaths2, function(x){ x$Sex <- sexLabel ; return(x)})
  
  # All-cause IGME rates (random draw)
  l_rates2 <- lapply(V_SAMPLE, function(x) as.data.frame(DRAWS_ENV$ratesAll[, , x]))
  l_rates2 <- lapply(l_rates2, function(x){ x$ISO3 <- rownames(x) ; return(x)})
  l_rates2 <- lapply(l_rates2, function(x){ x <- as.data.frame(pivot_longer(x, cols = 1:(ncol(x)-1),
                                                                              names_to = 'Year', values_to = 'Rate')) ; return(x)})
  l_rates2 <- lapply(l_rates2, function(x){ x$Sex <- sexLabel ; return(x)})
  
  # Merge
  l_sampled_draws <- mapply(function(x, y) merge(x, y, by = idVars, all=TRUE), x = l_deaths1, y = l_deaths2, SIMPLIFY = FALSE)
  l_sampled_draws <- mapply(function(x, y) merge(x, y, by = idVars, all=TRUE), x = l_sampled_draws, y = l_rates2, SIMPLIFY = FALSE)
  
  #------------------------#
  # 2023.06.09 PATCH
  
  # Avoid NA on draws
  l_sampled_draws <- lapply(l_sampled_draws, function(x){ x$Deaths2[which(is.na(x$Deaths2))] <- 0 ; return(x)})
  l_sampled_draws <- lapply(l_sampled_draws, function(x){ x$Deaths1[which(is.na(x$Deaths1))] <- 0 ; return(x)})
  l_sampled_draws <- lapply(l_sampled_draws, function(x){ x$Rate[which(is.na(x$Rate))] <- 0 ; return(x)})
  
  # END PATCH
  #------------------------#
  
  return(l_sampled_draws)
}

#drawIGME <- merge(deaths1, deaths2, by = idVars)
#drawIGME <- merge(drawIGME, rates2, by = idVars)
#head(Reduce(function(x, y) merge(x, y, all=TRUE), list(l_deaths1[1], l_deaths2[1], l_rates2[2])))

#fn_merge_3list <-function(list1, list2, list3){ Reduce(function(x, y) merge(x, y, by = idVars, all=TRUE), list(list1, list2, list3))}
#l_randdraw_igme <- mapply(fn_merge_3list, l_deaths1, l_deaths2, l_rates2, SIMPLIFY=FALSE)

#--------------------------------#
# UN IGME ENVELOPES: RANDOM DRAW #
#--------------------------------#

# i <- 1
# 
# # Crisis-free IGME deaths (random draw)
# deaths1 <- as.data.frame(L_DRAWS_IGME$deaths[, , randDraws[i]])
# deaths1$ISO3 <- rownames(deaths1)
# deaths1 <- as.data.frame(pivot_longer(deaths1, cols = 1:(ncol(deaths1)-1),
#                                       names_to = 'Year', values_to = 'Deaths1'))
# deaths1$Sex <- sexLabel
# 
# # All-cause IGME deaths (random draw)
# deaths2 <- as.data.frame(L_DRAWS_IGME$deathsAll[, , randDraws[i]])
# deaths2$ISO3 <- rownames(deaths2)
# deaths2 <- as.data.frame(pivot_longer(deaths2, cols = 1:(ncol(deaths2)-1),
#                                       names_to = 'Year', values_to = 'Deaths2'))
# deaths2$Sex <-  sexLabel
# 
# # All-cause IGME rates (random draw)
# rates2 <- as.data.frame(L_DRAWS_IGME$ratesAll[, , randDraws[i]])
# rates2$ISO3 <- rownames(rates2)
# rates2 <- as.data.frame(pivot_longer(rates2, cols = 1:(ncol(rates2)-1),
#                                      names_to = 'Year', values_to = 'Rate'))
# rates2$Sex <- sexLabel
# 
# # Merge estimates
# drawIGME <- merge(deaths1, deaths2, by = idVars)
# drawIGME <- merge(drawIGME, rates2, by = idVars)
# 
# #------------------------#
# # 2023.06.09 PATCH
# 
# # Avoid NA on draws
# drawIGME$Deaths2[which(is.na(drawIGME$Deaths2))] <- 0
# drawIGME$Deaths1[which(is.na(drawIGME$Deaths1))] <- 0
# drawIGME$Rate[which(is.na(drawIGME$Rate))] <- 0
# 
# # END PATCH
# #------------------------#


##################################################
####
####  Randomly assign goodvr/china for current draw: multinomial distribution
####
##################################################

#drawIGME <- draws_env_Sampled[[1]]
#dat <- csmf_GOODVR
#dat <- csmf_CHN
#key_cod

#DAT <- csmf_GOODVR
#CTRYGRP <- "GOODVR"
#KEY_COD <- key_cod
#DRAW_ENV <- draws_env_Sampled[[1]]

fn_rand_assign_vr <- function(DAT, DRAW_ENV, KEY_COD, CTRYGRP){
  
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

##################################################
####
####  Sample from log-normal distribution
####
##################################################

# FUNCION TO SAMPLE FROM A LOG-NORMAL DISTRIBUTION
fn_sample_log_norm <- function(mu, low, up) {
  
  ## mu       Mean value (reported point estimate)
  ## low      Lower bound of the 95% UI
  ## up       Upper bound of the 95% UI
  
  # Estimate normal SD
  s <- (up - low) / 3.92
  # Log-normal mean
  muLog <- log(mu^2 / sqrt(s^2 + mu^2))
  # Log-normal sd
  sLog <- sqrt(log(1 + (s^2 / mu^2)))
  # Draw random values
  randVal <- rlnorm(n = length(mu), meanlog = muLog, sdlog = sLog)
  
  # Output
  return(randVal)
  
}

##################################################
####
####  Randomly assign measles for current draw: log normal distribution
####
##################################################

fn_rand_assign_meas <- function(DAT){

  # Cap the upper bound of Measles (EPI + END) to the envelope
  DAT$msl_ub[which(DAT$msl_ub > DAT$Deaths2)] <- DAT$Deaths2[which(DAT$msl_ub > DAT$Deaths2)]
  
  # UNCERTAINTY INTERVALS only available for ENDEMIC + EPIDEMIC Measles
  # Random Epidemic + endemic Measles
  
  # Randomly sample value for total measles
  v_meas_all <- rep(0, nrow(DAT))
  v_idSqz <- which(DAT$meas_all != 0)
  if(length(v_idSqz) > 0){
    # Sample from log normal distribution
    v_meas_all[v_idSqz] <- fn_sample_log_norm(mu = DAT$meas_all[v_idSqz],
                                    low = DAT$msl_lb[v_idSqz],
                                    up = DAT$msl_ub[v_idSqz])
    # Save point estimate for endemic measles
    v_meas_end <- DAT$Measles
    v_idSqz <- which(DAT$Measles != 0)
    # Calculate random endemic measles by subtracting epidemic from random total
    DAT$Measles[v_idSqz] <- v_meas_all[v_idSqz] - DAT$meas_epi[v_idSqz] 
    # Avoid negative values
    DAT$Measles[which(DAT$Measles < 0)] <- 0
    # Calculate random epidemic measles by subtracting endemic point estimate from random total
    v_idSqz <- which(DAT$meas_epi != 0)
    DAT$meas_epi[v_idSqz] <- v_meas_all[v_idSqz] - v_meas_end[v_idSqz]
    # Note: Measles and meas_epi do not need to add up to meas_all, but their total should be within msl_lb and msl_ub
    ## meas_epi will be added on top of envelope
  }
  
  return(DAT)
  
}

##################################################
####
####  Randomly assign TB for current draw: log normal distribution
####
##################################################

#DAT <- draws_csmf_AddSinglecause[[1]]

fn_rand_assign_tb <- function(DAT){
  
  # Cap the upper bound of TB to the envelope
  DAT$tb_ub[which(DAT$tb_ub > DAT$Deaths1)] <- DAT$Deaths1[which(DAT$tb_ub > DAT$Deaths1)]
  # Values to be squeezed
  v_idSqz <- which(DAT$TB != 0 & DAT$TB > DAT$tb_lb & DAT$TB < DAT$tb_ub)
  # Sample random values
  if (length(v_idSqz) > 0) {
    DAT$TB[v_idSqz] <- fn_sample_log_norm(mu = DAT$TB[v_idSqz],
                                   low = DAT$tb_lb[v_idSqz],
                                   up = DAT$tb_ub[v_idSqz])
  }
  
  if(respTB){
    
    # Cap the upper bound of TB to the envelope
    DAT$tbre_ub[which(DAT$tbre_ub > DAT$Deaths1)] <- DAT$Deaths1[which(DAT$tbre_ub > DAT$Deaths1)]
    # Values to be squeezed
    v_idSqz <- which(DAT$TBre != 0 & DAT$TBre > DAT$tbre_lb & DAT$TBre < DAT$tbre_ub)
    # Sample random values
    if (length(v_idSqz) > 0) {
      DAT$TBre[v_idSqz] <- fn_sample_log_norm(mu = DAT$TBre[v_idSqz],
                                       low = DAT$tbre_lb[v_idSqz],
                                       up = DAT$tbre_ub[v_idSqz])
    }
  }
  
  return(DAT)
}


##################################################
####
####  Randomly assign HIV for current draw: truncated normal distribution
####
##################################################

fn_rand_assign_hiv <- function(DAT){
  
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

