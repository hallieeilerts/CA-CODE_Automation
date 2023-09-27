fn_capMalFrac <- function(CSMF, DAT_MALARIA_5TO19, FRAC_MALARIA_01to04){
  
  #' @title Cap malaria fractions
  # 
  #' @description Caps malaria fractions at country-year-specific fraction level for 1-4y
  #
  #' @param CSMF Data frame with predicted CSMFs.
  #' @param DAT_MALARIA_5TO19 Data frame with columns c("ISO3", "Year", "dth_malaria_5to19")
  #' @param FRAC_MALARIA_01to04 Data frame with final estimated CSMF for malaria for 1-4y
  #' @return Data frame where predicted CSMFs have been adjusted for capped malaria fractions.
  
  # Identify COD
  v_cod <- names(CSMF)
  v_cod <- v_cod[!(v_cod %in% idVars)]
  
  ### Merge on malaria deaths
  
  dat <- merge(CSMF, DAT_MALARIA_5TO19, by = c("ISO3", "Year"), all.x = T)
  
  # Identify cases with 0 malaria
  idMal <- which(dat$dth_malaria_5to19 == 0 | is.na(dat$dth_malaria_5to19))
  
  # Force malaria to be 0
  if (length(idMal) > 0) {
    dat[idMal, paste(v_cod)] <- dat[idMal, paste(v_cod)]/rowSums(dat[idMal, paste(v_cod[v_cod != "Malaria"])])
    dat[idMal, "Malaria"] <- 0
  }
  
  # Remove unnecessary columns
  v_remove <- names(DAT_MALARIA_5TO19)[!(names(DAT_MALARIA_5TO19) %in% idVars)]
  dat <- dat[, !(names(dat) %in% v_remove)]
  
  ### Merge on malaria 1-59 month CSMF
  
  dat <- merge(dat, FRAC_MALARIA_01to04, by = c("ISO3", "Year"), all.x = T)
  
  # Assign 0 to NA (if any)
  dat$csmf_malaria_01to04[which(is.na(dat$csmf_malaria_01to04))] <- 0
  
  # Identify cases to update malaria
  idMal <- which(dat$Malaria > dat$csmf_malaria_01to04)
  
  # Cap malaria
  if (length(idMal) > 0) {
    dat$Malaria[idMal] <- dat$csmf_malaria_01to04[idMal]
    idCod <- v_cod[v_cod != "Malaria"]
    dat[idMal, paste(idCod)] <- dat[idMal, paste(idCod)] / rowSums(dat[idMal, paste(paste(idCod))])
    dat[idMal, paste(idCod)] <- dat[idMal, paste(idCod)] * (1 - dat$Malaria[idMal])
    rm(idCod)
  }
  
  # Remove unnecessary data
  v_remove <- names(FRAC_MALARIA_01to04)[!(names(FRAC_MALARIA_01to04) %in% idVars)]
  dat <- dat[, !(names(dat) %in% v_remove)]
  
  # Tidy up
  dat <- dat[order(dat$ISO3, dat$Year),]
  
  return(dat)
  
}
