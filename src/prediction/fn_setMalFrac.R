fn_setMalFrac <- function(CSMF){
  
  #' @title Set malaria fractions
  # 
  #' @description Adds a column for malaria fraction and sets to zero.
  #
  #' @param CSMF Data frame with predicted CSMFs, of which malaria is not included.
  #' @return Dataframe with predicted CSMFs plus a new column for Malaria where all CSMFs are zero.
  
  CSMF$Malaria <- 0
  
  return(CSMF)
}
