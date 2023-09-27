fn_randAssignVR <- function(CSMF, KEY_COD, CTRYGRP){
  
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
