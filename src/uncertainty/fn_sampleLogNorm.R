fn_sampleLogNorm <- function(MU, LB, UB){
  
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