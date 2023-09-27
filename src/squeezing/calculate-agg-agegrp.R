################################################################################
#' @description Calculate deaths and rates for aggregate age groups
#' @return Data frame for age-sex group of interest with deaths and rates
################################################################################
#' Libraries

#' Inputs
source("./src/prepare-session/set-inputs.R")
source("./src/prepare-session/create-session-variables.R")
nat05to09 <- read.csv(paste("./gen/squeezing/output/csmfSqz_05to09.csv", sep = ""))
nat10to14 <- read.csv(paste("./gen/squeezing/output/csmfSqz_10to14.csv", sep = ""))
nat15to15f <- read.csv(paste("./gen/squeezing/output/csmfSqz_15to19f.csv", sep = ""))
nat15to15m <- read.csv(paste("./gen/squeezing/output/csmfSqz_15to19m.csv", sep = ""))
#reg05to09 <- read.csv(paste("./gen/results/output/PointEstimates_Regional_05to09_", resDate, ".csv", sep =""))
#reg10to14 <- read.csv(paste("./gen/results/output/PointEstimates_Regional_10to14_", resDate, ".csv", sep =""))
#reg15to19f <- read.csv(paste("./gen/results/output/PointEstimates_Regional_15to19f_", resDate, ".csv", sep =""))                     
#reg15to19m <- read.csv(paste("./gen/results/output/PointEstimates_Regional_15to19m_", resDate, ".csv", sep =""))     
env_crisisincl_u20 <- read.csv("./gen/data-management/output/env_crisisIncl_u20.csv")
key_region <- read.csv("./gen/data-management/output/key_region_u20.csv")
key_ctryclass <- read.csv("./gen/data-management/output/key_ctryclass_u20.csv")
################################################################################

# Calculate aggregate rates
nat05to14 <- fn_calc_agg_rate(5,14, codAll, dat05to09 = nat05to09, dat10to14 = nat10to14)
nat05to19 <- fn_calc_agg_rate(5,19, codAll, env_crisisincl_u20, dat05to09 = nat05to09, dat10to14 = nat10to14, dat15to19f = nat15to19f, dat15to19m = nat15to19m)
nat10to19 <- fn_calc_agg_rate(10,19, codAll, env_crisisincl_u20, dat10to14 = nat10to14, dat15to19f = nat15to19f, dat15to19m = nat15to19m)
nat15to19 <- fn_calc_agg_rate(15,19, codAll, env_crisisincl_u20, dat15to19f = nat15to19f, dat15to19m = nat15to19m)

# Format
nat05to14 <- fn_format_results(nat05to14, key_region, key_ctryclass, codAll)
nat05to19 <- fn_format_results(nat05to19, key_region, key_ctryclass, codAll)
nat10to19 <- fn_format_results(nat10to19, key_region, key_ctryclass, codAll)
nat15to19 <- fn_format_results(nat15to19, key_region, key_ctryclass, codAll)

# Calculate aggregate rates
#reg05to14 <- fn_calc_agg_rate(5,14, codAll, dat05to09 = reg05to09, dat10to14 = reg10to14)
#reg05to19 <- fn_calc_agg_rate(5,19, codAll, env_crisisincl_u20, dat05to09 = reg05to09, dat10to14 = reg10to14, dat15to19f = reg15to19f, dat15to19m = reg15to19m)
#reg10to19 <- fn_calc_agg_rate(10,19, codAll, env_crisisincl_u20, dat10to14 = reg10to14, dat15to19f = reg15to19f, dat15to19m = reg15to19m)
#reg15to19 <- fn_calc_agg_rate(15,19, codAll, env_crisisincl_u20, dat15to19f = reg15to19f, dat15to19m = reg15to19m)

# Format
reg05to14 <- fn_format_results(reg05to14, key_region, key_ctryclass, codAll)
reg05to19 <- fn_format_results(reg05to19, key_region, key_ctryclass, codAll)
reg10to19 <- fn_format_results(reg10to19, key_region, key_ctryclass, codAll)
reg15to19 <- fn_format_results(reg15to19, key_region, key_ctryclass, codAll)

# Save output(s)
#write.csv(nat05to14, paste("./gen/results/output/PointEstimates_National_05to14_", format(Sys.Date(), format="%Y%m%d"),".csv", sep=""), row.names = FALSE)
#write.csv(nat05to19, paste("./gen/results/output/PointEstimates_National_05to19_", format(Sys.Date(), format="%Y%m%d"),".csv", sep=""), row.names = FALSE)
#write.csv(nat10to19, paste("./gen/results/output/PointEstimates_National_10to19_", format(Sys.Date(), format="%Y%m%d"),".csv", sep=""), row.names = FALSE)
#write.csv(nat15to19, paste("./gen/results/output/PointEstimates_National_15to19_", format(Sys.Date(), format="%Y%m%d"),".csv", sep=""), row.names = FALSE)

# AGELB <- 5
# AGEUB <- 14
# CODALL <- codAll
# ENV <- env_crisisincl_u20
# DAT_5TO9 <- nat05to09
# DAT_10TO14 <- nat10to14
# DAT_15TO19F <- nat15to15f
# DAT_15TO19M <- nat15to15m
# REGIONAL <- FALSE

fn_calc_agg_rate <- function(DAT_5TO9 = NULL,  DAT_10TO14 = NULL, DAT_15TO19F = NULL, DAT_15TO19M = NULL, ENV, AGELB, AGEUB, CODALL, REGIONAL = FALSE){
  
  #' @title Calculate deaths, rates, CSMFs for non-standard age intervals
  # 
  #' @description Calculates point estimates for aggregate age groups.
  #
  #' @param AGELB Integer denoting lower bound of age group (possible values: 5, 10, 15)
  #' @param AGEUB Integer denoting upper bound of age group  (possible values: 9, 14, 19)
  #' @param CODALL Vector with CODs for all age groups in correct order.
  #' @param ENV   Data frame IGME envelopes for crisis-included deaths and rates for all ages.
  #' @param CSMF[age-group]  Data frame of age-specific CSMFs in final format for sharing (published).
  #' @return Data frame with all identifying columns and point estimates for aggregate age groups.
  
  env <- ENV
  v_cod <- CODALL
  dat5to9 <- DAT_5TO9
  dat10to14 <- DAT_10TO14
  dat15to19f <- DAT_15TO19F
  dat15to19m <- DAT_15TO19M
  
  # Aux idVars
  if(!REGIONAL){
    idVarsAux <- idVars
  }else{
    idVarsAux <- c("Region", idVars[2:3])
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
    # Get sex-combined all-cause crisis-included rate from IGME envelope
    env <- subset(env, Sex == sexLabels[1] & AgeLow == 15 & AgeUp == 19)[,c("ISO3", "Year", "Rate2")]
  }
  
  # Merge all-cause deaths and rates for different age groups
  v_cols1 <- c("ISO3","Year","Deaths1","Rate1","Deaths2","Rate2")
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
  names(df_envAgg)[names(df_envAgg) == "Rate1.x"] <- "Rate1_df1" 
  names(df_envAgg)[names(df_envAgg) == "Rate1.y"] <- "Rate1_df2"
  names(df_envAgg)[names(df_envAgg) == "Rate1"] <- "Rate1_df3"
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
    names(df_envAgg)[names(df_envAgg) == "Deaths1_df3"] <- "Deaths1"
    names(df_envAgg)[names(df_envAgg) == "Rate1_df3"] <- "Rate1"
    names(df_envAgg)[names(df_envAgg) == "Deaths2_df3"] <- "Deaths2"
    names(df_envAgg)[names(df_envAgg) == "Rate2_df3"] <- "Rate2"
  }
  if(AGEUB - AGELB == 9){
    df_envAgg$Rate1 <- 1000 - (1000 - df_envAgg$Rate1_df1)*(1 - df_envAgg$Rate1_df2 / 1000)
    df_envAgg$Deaths1 <- df_envAgg$Deaths1_df1 + df_envAgg$Deaths1_df2
    df_envAgg$Rate2 <- 1000 - (1000 - df_envAgg$Rate2_df1)*(1 - df_envAgg$Rate2_df2 / 1000)
    df_envAgg$Deaths2 <- df_envAgg$Deaths2_df1 + df_envAgg$Deaths2_df2
  }
  if(AGEUB - AGELB == 14){
    df_envAgg$Rate1 <- 1000 - (1000 - df_envAgg$Rate1_df1)*(1 - df_envAgg$Rate1_df2 / 1000)*(1 - df_envAgg$Rate1_df3 / 1000)
    df_envAgg$Deaths1 <- df_envAgg$Deaths1_df1 + df_envAgg$Deaths1_df2 + df_envAgg$Deaths1_df3
    df_envAgg$Rate2 <- 1000 - (1000 - df_envAgg$Rate2_df1)*(1 - df_envAgg$Rate2_df2 / 1000)*(1 - df_envAgg$Rate2_df3 / 1000)
    df_envAgg$Deaths2 <- df_envAgg$Deaths2_df1 + df_envAgg$Deaths2_df2 + df_envAgg$Deaths2_df3
  }
  df_envAgg <- df_envAgg[, v_cols1]
  
  # Remove old all-cause crisis-included deaths and rate columns from CSMFs data
  # Add zero deaths for missing COD
  # Rbind each age group together
  v_cols2 <- c("Deaths1","Rate1","Deaths2","Rate2")
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

