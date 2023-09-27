fn_checkCSMFsqz <- function(CSMF, KEY_COD){
  
  #' @title Check if squeezed CSMFs add up to 1 or contain NAs
  # 
  #' @description Checks for country-years where squeezed fractions do not add up to 1.
  #
  #' @param CSMF Data frame with CSMFs that have been processed by squeezing functions, all-cause crisis-free and crisis-included deaths and rates.
  #' @param KEY_COD Data frame with age-specific CODs with different levels of classification.
  #' @return Data frame with rows where fractions for country-year do not add up to 1 or contain an NA.
  
  dat <- CSMF
  
  v_cod <- unique(KEY_COD$Reclass)  # Vector with ALL CAUSES OF DEATH (including single-cause estimates)
  v_cod <- v_cod[!v_cod %in% c("Other", "Undetermined")]
  
  v_containsNA <- which(is.na(rowSums(dat[, paste(v_cod)])))
  v_sumnot1    <- which(round(rowSums(dat[, paste(v_cod)]),5) != 1)
  v_audit      <- unique(v_containsNA, v_sumnot1)
  v_audit      <- sort(v_audit)
  
  # Checks
  if(any(is.na(rowSums(dat[, paste(v_cod)])))){
    warning("CSMFs contain NA")
  }
  if(any(round(rowSums(dat[, paste(v_cod)]),5) != 1)){
    warning("CSMFs do not add up to 1")
  }
  #print(round(rowSums(DAT[, paste(v_cod)]),7), digits = 20)
  #table(rowSums(DAT[, paste(v_cod)]))
  #table(round(rowSums(DAT[, paste(v_cod)]),5))
  #DAT[which(round(rowSums(DAT[, paste(v_cod)]),5) == 0.99942),]
  #DAT[which(rowSums(DAT[, paste(v_cod)]) != 1),]
  #foo <- DAT[which(rowSums(DAT[, paste(v_cod)]) > 1.18),]
  #foo[, c("ISO3",paste(v_cod))]
  
  dat <- dat[c(v_audit),]
  dat$csmf_SUM <- round(rowSums(dat[, paste(v_cod)]),5)
  rownames(dat) <- NULL
  
  return(dat)
}