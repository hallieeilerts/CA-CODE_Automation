fn_callCalcAggAges <- function(AGELB, AGEUB, CODALL, CSMF_5TO9 = NULL, CSMF_10TO14 = NULL, CSMF_15TO19F = NULL, CSMF_15TO19M = NULL, ENV = NULL, REGIONAL = FALSE, UNCERTAINTY = FALSE){
  
  # #
  # AGELB=15
  # AGEUB=19
  # CODALL=codAll
  # CSMF_5TO9 = csmfSqzDraws_05to09
  # CSMF_10TO14 = csmfSqzDraws_10to14
  # CSMF_15TO19F = csmfSqzDraws_15to19f
  # CSMF_15TO19M = csmfSqzDraws_15to19m
  # ENV = envDraws_SAMP_15to19
  # UNCERTAINTY = TRUE
  # REGIONAL = FALSE
  
  env <- ENV
  dat5to9 <- CSMF_5TO9
  dat10to14 <- CSMF_10TO14
  dat15to19f <- CSMF_15TO19F
  dat15to19m <- CSMF_15TO19M
  
  if(UNCERTAINTY){
    if(AGELB == 5){
      l_ageLB <- rep(list(AGELB), length(CSMF_5TO9))
      l_ageUB <- rep(list(AGEUB), length(CSMF_5TO9))
      l_codAll <- rep(list(CODALL), length(CSMF_5TO9))
      l_regional <- rep(list(REGIONAL), length(CSMF_5TO9))
      l_uncertainty <- rep(list(UNCERTAINTY), length(CSMF_5TO9))
    }
    if(AGELB == 10){
      l_ageLB <- rep(list(AGELB), length(CSMF_10TO14))
      l_ageUB <- rep(list(AGEUB), length(CSMF_10TO14))
      l_codAll <- rep(list(CODALL), length(CSMF_10TO14))
      l_regional <- rep(list(REGIONAL), length(CSMF_10TO14))
      l_uncertainty <- rep(list(UNCERTAINTY), length(CSMF_10TO14))
    }
    if(AGELB == 15){
      l_ageLB <- rep(list(AGELB), length(CSMF_15TO19M))
      l_ageUB <- rep(list(AGEUB), length(CSMF_15TO19M))
      l_codAll <- rep(list(CODALL), length(CSMF_15TO19M))
      l_regional <- rep(list(REGIONAL), length(CSMF_15TO19M))
      l_uncertainty <- rep(list(UNCERTAINTY), length(CSMF_15TO19M))
    }
  }
  
  l_res <- mapply(function(a,b,c,d,e,f,g,h,i,j) fn_calcAggAges(a,b,c,d,e,f,g,h,i,j), l_ageLB, l_ageUB, l_codAll, dat5to9, dat10to14, dat15to19f, dat15to19m, env, l_regional, l_uncertainty, SIMPLIFY = FALSE)
  
  return(l_res)
} 
