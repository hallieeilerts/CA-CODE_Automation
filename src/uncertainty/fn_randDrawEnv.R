fn_randDrawEnv <- function(L_ENVDRAWS, V_SAMPLE){
  
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
