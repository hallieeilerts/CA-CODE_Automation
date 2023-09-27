fn_addAgeCol <- function(DAT, AGELB, AGEUB){
  
  dat <- DAT
  dat$AgeLow <- AGELB
  dat$AgeUp <- AGEUB
  
  return(dat)
}