fn_calcAggAges <- function(AGELB, AGEUB, CODALL, CSMF_5TO9 = NULL, CSMF_10TO14 = NULL, CSMF_15TO19F = NULL, CSMF_15TO19M = NULL, ENV = NULL, REGIONAL = FALSE, UNCERTAINTY = FALSE){
  
  #' @title Calculate deaths, rates, CSMFs for non-standard age intervals
  # 
  #' @description Calculates point estimates for aggregate age groups.
  #' 
  #' @param DAT[age-group] Data frame with CSMFs that have been processed by squeezing functions, CSMFs that were not squeezed (GOODVR), all-cause crisis-free and crisis-included deaths and rates.
  #' @param AGELB Integer denoting lower bound of age group (possible values: 5, 10, 15)
  #' @param AGEUB Integer denoting upper bound of age group  (possible values: 9, 14, 19)
  #' @param CODALL Vector with CODs for all age groups in correct order.
  #' @param ENV Data frame IGME envelopes with crisis-included deaths and rates for all ages.
  #' @return Data frame with CSMFs and all-cause crisis-free and crisis-included deaths and rates for aggregate age groups
  
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
  v_cod <- CODALL
  dat5to9 <- CSMF_5TO9
  dat10to14 <- CSMF_10TO14
  dat15to19f <- CSMF_15TO19F
  dat15to19m <- CSMF_15TO19M
  
  # Aux idVars
  if(!REGIONAL){
    idVarsAux <- idVars
  }else{
    idVarsAux <- c("Region", idVars[2:3])
  }
  
  if(!UNCERTAINTY){
    v_cols1 <- c("ISO3","Year","Deaths1","Rate1","Deaths2","Rate2")
  }else{
    v_cols1 <- c("ISO3","Year","Deaths1","Deaths2","Rate2")
  }
  
  
  # Convert fractions into deaths
  if(AGELB == 5){
    dat5to9[, v_cod[v_cod %in% names(dat5to9)]] <- dat5to9[, v_cod[v_cod %in% names(dat5to9)]] * dat5to9$Deaths2
    dat10to14[, v_cod[v_cod %in% names(dat10to14)]] <- dat10to14[, v_cod[v_cod %in% names(dat10to14)]] * dat10to14$Deaths2
  }
  if(AGELB == 10){
    dat10to14[, v_cod[v_cod %in% names(dat10to14)]] <- dat10to14[, v_cod[v_cod %in% names(dat10to14)]] * dat10to14$Deaths2
  }
  if(AGEUB == 19){
    dat15to19f[, v_cod[v_cod %in% names(dat15to19f)]] <- dat15to19f[, v_cod[v_cod %in% names(dat15to19f)]] * dat15to19f$Deaths2
    dat15to19m[, v_cod[v_cod %in% names(dat15to19m)]] <- dat15to19m[, v_cod[v_cod %in% names(dat15to19m)]] * dat15to19m$Deaths2
    # Combine sexes
    dat15to19 <- dat15to19f
    dat15to19[, v_cod[v_cod %in% names(dat15to19)]] <- dat15to19[, v_cod[v_cod %in% names(dat15to19)]] + dat15to19m[, v_cod[v_cod %in% names(dat15to19m)]]
    dat15to19$Sex <- sexLabels[1]
    # Get sex-combined deaths and rates from IGME envelope
    # For envelope draws, no need to subset, it will only be the 15-19 deaths and rates
    if(!UNCERTAINTY){
      env <- subset(env, Sex == sexLabels[1] & AgeLow == 15 & AgeUp == 19)[,v_cols1]
    }
  }
  
  # Merge all-cause deaths and rates for different age groups
  if(AGELB == 5 & AGEUB == 19){
    l_df <- list(dat5to9[, v_cols1], dat10to14[, v_cols1], env[, v_cols1])
  }
  if(AGELB == 5 & AGEUB == 14){
    l_df <- list(dat5to9[, v_cols1], dat10to14[, v_cols1])
  }
  if(AGELB == 10 & AGEUB == 19){
    l_df <- list(dat10to14[, v_cols1], env[, v_cols1])
  }
  if(AGELB == 15 & AGEUB == 19){
    l_df <- list(env)
  }
  if(length(l_df)>1){ 
    df_envAgg <- Reduce(function(x, y) merge(x, y, by = c("ISO3", "Year"), all=TRUE), l_df)
  }else{
    df_envAgg <- l_df[[1]]
  }
  if(!UNCERTAINTY){
    names(df_envAgg)[names(df_envAgg) == "Rate1.x"] <- "Rate1_df1" 
    names(df_envAgg)[names(df_envAgg) == "Rate1.y"] <- "Rate1_df2"
    names(df_envAgg)[names(df_envAgg) == "Rate1"] <- "Rate1_df3"
  }
  names(df_envAgg)[names(df_envAgg) == "Deaths1.x"] <- "Deaths1_df1" 
  names(df_envAgg)[names(df_envAgg) == "Deaths1.y"] <- "Deaths1_df2"
  names(df_envAgg)[names(df_envAgg) == "Deaths1"] <- "Deaths1_df3"
  names(df_envAgg)[names(df_envAgg) == "Rate2.x"] <- "Rate2_df1" 
  names(df_envAgg)[names(df_envAgg) == "Rate2.y"] <- "Rate2_df2"
  names(df_envAgg)[names(df_envAgg) == "Rate2"] <- "Rate2_df3"
  names(df_envAgg)[names(df_envAgg) == "Deaths2.x"] <- "Deaths2_df1" 
  names(df_envAgg)[names(df_envAgg) == "Deaths2.y"] <- "Deaths2_df2"
  names(df_envAgg)[names(df_envAgg) == "Deaths2"] <- "Deaths2_df3"
  
  # Calculate all-cause deaths and rates for aggregate age groups
  if(AGEUB - AGELB == 4){
    if(!UNCERTAINTY){
      names(df_envAgg)[names(df_envAgg) == "Rate1_df3"] <- "Rate1"
    }
    names(df_envAgg)[names(df_envAgg) == "Deaths1_df3"] <- "Deaths1"
    names(df_envAgg)[names(df_envAgg) == "Deaths2_df3"] <- "Deaths2"
    names(df_envAgg)[names(df_envAgg) == "Rate2_df3"] <- "Rate2"
  }
  if(AGEUB - AGELB == 9){
    if(!UNCERTAINTY){
      df_envAgg$Rate1 <- 1000 - (1000 - df_envAgg$Rate1_df1)*(1 - df_envAgg$Rate1_df2 / 1000)
    }
    df_envAgg$Deaths1 <- df_envAgg$Deaths1_df1 + df_envAgg$Deaths1_df2
    df_envAgg$Rate2 <- 1000 - (1000 - df_envAgg$Rate2_df1)*(1 - df_envAgg$Rate2_df2 / 1000)
    df_envAgg$Deaths2 <- df_envAgg$Deaths2_df1 + df_envAgg$Deaths2_df2
  }
  if(AGEUB - AGELB == 14){
    if(!UNCERTAINTY){
      df_envAgg$Rate1 <- 1000 - (1000 - df_envAgg$Rate1_df1)*(1 - df_envAgg$Rate1_df2 / 1000)*(1 - df_envAgg$Rate1_df3 / 1000)
    }
    df_envAgg$Deaths1 <- df_envAgg$Deaths1_df1 + df_envAgg$Deaths1_df2 + df_envAgg$Deaths1_df3
    df_envAgg$Rate2 <- 1000 - (1000 - df_envAgg$Rate2_df1)*(1 - df_envAgg$Rate2_df2 / 1000)*(1 - df_envAgg$Rate2_df3 / 1000)
    df_envAgg$Deaths2 <- df_envAgg$Deaths2_df1 + df_envAgg$Deaths2_df2 + df_envAgg$Deaths2_df3
  }
  df_envAgg <- df_envAgg[, v_cols1]
  
  # Remove old all-cause crisis-included deaths and rate columns from CSMFs data
  # Add zero deaths for missing COD
  # Rbind each age group together
  if(!UNCERTAINTY){
    v_cols2 <- c("Deaths1","Rate1","Deaths2","Rate2")
  }else{
    v_cols2 <- c("Deaths1","Deaths2","Rate2")
  }
  if(AGELB == 5 & AGEUB == 19){
    dat5to9 <- dat5to9[, !names(dat5to9) %in% v_cols2]
    dat10to14 <- dat10to14[, !names(dat10to14) %in% v_cols2]
    dat15to19 <- dat15to19[, !names(dat15to19) %in% v_cols2]
    addCOD <- v_cod[which(!v_cod %in% names(dat5to9))]
    dat5to9[, paste(addCOD)] <- 0
    addCOD <- v_cod[which(!v_cod %in% names(dat10to14))]
    dat10to14[, paste(addCOD)] <- 0
    addCOD <- v_cod[which(!v_cod %in% names(dat15to19))]
    dat15to19[, paste(addCOD)] <- 0
    dat <- rbind(dat5to9, dat10to14, dat15to19)
  }
  if(AGELB == 5 & AGEUB == 14){
    dat5to9 <- dat5to9[, !names(dat5to9) %in% v_cols2]
    dat10to14 <- dat10to14[, !names(dat10to14) %in% v_cols2]
    addCOD <- names(dat10to14)[!(names(dat10to14) %in% names(dat5to9))]
    dat5to9[, paste(addCOD)] <- 0
    addCOD <- names(dat5to9)[!(names(dat5to9) %in% names(dat10to14))]
    dat10to14[, paste(addCOD)] <- 0
    dat <- rbind(dat5to9, dat10to14)
  }
  if(AGELB == 10 & AGEUB == 19){
    dat10to14 <- dat10to14[, !names(dat10to14) %in% v_cols2]
    dat15to19 <- dat15to19[, !names(dat15to19) %in% v_cols2]
    addCOD <- names(dat15to19)[!(names(dat15to19) %in% names(dat10to14))]
    dat10to14[, paste(addCOD)] <- 0
    addCOD <- names(dat10to14)[!(names(dat10to14) %in% names(dat15to19))]
    dat15to19[, paste(addCOD)] <- 0
    dat <- rbind(dat10to14, dat15to19)
  }
  if(AGELB == 15 & AGEUB == 19){
    dat15to19 <- dat15to19[, !names(dat15to19) %in% v_cols2]
    dat <- dat15to19
  }
  
  # Sum deaths over aggregate age groups
  if(!REGIONAL){
    dat <- aggregate(dat[, v_cod[v_cod %in% names(dat)]], by = list(dat$ISO3, dat$Year, dat$Sex), sum)
  }else{
    dat <- aggregate(dat[, v_cod[v_cod %in% names(dat)]], by = list(dat$Region, dat$Year, dat$Sex), sum)
  }
  names(dat)[1:3] <- idVarsAux
  
  # Merge columns with all-cause deaths and rates for aggregate age groups
  dat <- merge(dat, df_envAgg, by = c("ISO3", "Year"), all.x = T, all.y = F)
  
  # Back transform into fractions
  dat[, v_cod[v_cod %in% names(dat)]] <- round(dat[, v_cod[v_cod %in% names(dat)]] / rowSums(dat[, v_cod[v_cod %in% names(dat)]]), 5)
  
  # Tidy up
  dat <- dat[order(dat$ISO3, dat$Year), ]
  rownames(dat) <- NULL
  
  return(dat)
}
