fn_sqzCrisisEpi <- function(CSMF, KEY_COD){
  
  #' @title Squeeze epidemic crisis deaths
  # 
  #' @description Transform fractions to deaths by multiplying by crisis-free deaths. Calculate fractions for epidemic crisis single-cause deaths (epi_colvio, epi_natdis) from this sum. Multiply fractions by difference between crisis-free and crisis-included envelopes. Add epidemic crisis single-cause deaths to endemic crises (CollectVio, NatDis). If there were no epidemic crisis single-cause deaths but there is a difference between crisis-free and crisis-included envelopes, divide all deaths by crisis-included envelope, normalize (distributing epi_allcause pro-rata), and convert back to deaths.
  #' 
  #' @param CSMF Data frame with CSMFs that has been prepared for squeezing.
  #' @param KEY_COD Data frame with age-specific CODs with different levels of classification.
  #' @return Data frame where deaths have been adjusted for epidemic crisis squeezing.
  
  dat <- CSMF
  
  # Vector with all causes of death (including single-cause estimates)
  v_cod <- unique(KEY_COD$Reclass)  
  v_cod <- v_cod[!v_cod %in% c("Other", "Undetermined")]
  
  # Transform fractions into deaths
  dat[, paste(v_cod)] <- dat[, paste(v_cod)] * dat$Deaths1
  
  # Identify country/years where there are epidemic deaths
  v_idEpi <- which(dat$epi_colvio + dat$epi_natdis != 0)
  
  # Calculate proportion of epidemic deaths that are colvio versus natdis
  if(length(v_idEpi) > 0){
    dat[v_idEpi, c("epi_colvio", "epi_natdis")] <- dat[v_idEpi, c("epi_colvio", "epi_natdis")]/(dat$epi_colvio[v_idEpi] + dat$epi_natdis[v_idEpi])  
  }
  
  # Distribute epidemic deaths proportionally by cause
  # Multiply proportion of colvio/natdis deaths by difference between all-cause and crisis-free envelopes
  # Add to endemic deaths for those causes.
  dat$CollectVio <- dat$CollectVio + dat$epi_colvio * (dat$Deaths2 - dat$Deaths1)
  dat$NatDis <- dat$NatDis + dat$epi_natdis * (dat$Deaths2 - dat$Deaths1)
  # dat$OtherCMPN <- dat$OtherCMPN + dat$epi_othercd * (dat$Deaths2 - dat$Deaths1)
  
  # Distribute epidemic deaths attributed to all causes pro-rata
  v_idEpi <- which(dat$epi_colvio + dat$epi_natdis == 0 & dat$Deaths2 > dat$Deaths1)
  if(length(v_idEpi) > 0){
    # Using crisis-included envelope, convert deaths to CSMFs
    dat[v_idEpi, paste(v_cod)] <- dat[v_idEpi, paste(v_cod)] / dat$Deaths2[v_idEpi]
    # These CSMFs will not add up to 1 because the all-cause epidemic deaths were not included in the numerator. 
    # Normalize the CSMFs so they add up to 1.
    dat[v_idEpi, paste(v_cod)] <- dat[v_idEpi, paste(v_cod)] / rowSums(dat[v_idEpi, paste(v_cod)], na.rm = T)
    # Convert fractions back to deaths using crisis-included envelope
    dat[v_idEpi, paste(v_cod)] <- dat[v_idEpi, paste(v_cod)] * dat$Deaths2[v_idEpi]
  }
  return(dat)
  
}