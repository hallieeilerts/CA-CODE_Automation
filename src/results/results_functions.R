

##################################################
####
####   Calculate uncertainty intervals from draws
####
##################################################

# NOTE: make this usable on regions
# make different funciton for AARR

#test <- mortDraws[1:5]
#UI = .95
#res <- fn_calc_ui(test)

fn_calc_ui <- function(DRAWS_CSMF, UI){
  
  # Create interval
  UI <- 1/2 + c(- UI, UI) / 2
  
  # Causes of death for this age group
  v_cod <- codAll[codAll %in% names(DRAWS_CSMF[[1]])]
  
  # Data frame with identifying columns
  df_idcols <- DRAWS_CSMF[[1]][, !names(DRAWS_CSMF[[1]]) %in% c('Deaths', 'Rate', paste(v_cod))]
  
  # Create lists for fractions, rates, deaths
  l_frac <- DRAWS_CSMF
  l_rates <- lapply(DRAWS_CSMF, function(x){ x[,v_cod] * x[,"Rate"] })
  l_deaths <- lapply(DRAWS_CSMF, function(x){ x[,v_cod] * x[,"Deaths"] })
  
  # Convert each data.frame in list to matrix that includes only CODs
  l_frac <- lapply(l_frac, function(x) as.matrix(x[,v_cod]))
  l_rates <- lapply(l_rates, function(x) as.matrix(x[,v_cod]))
  l_deaths <- lapply(l_deaths, function(x) as.matrix(x[,v_cod]))
  
  # Convert list to array
  # Calculate quantiles for each cell across array
  a_frac_lb <- apply(simplify2array(l_frac), c(1,2), quantile, UI[1], na.rm = T)
  a_frac_ub <- apply(simplify2array(l_frac), c(1,2), quantile, UI[2], na.rm = T)
  a_rates_lb <- apply(simplify2array(l_rates), c(1,2), quantile, UI[1], na.rm = T)
  a_rates_ub <- apply(simplify2array(l_rates), c(1,2), quantile, UI[2], na.rm = T)
  a_deaths_lb <- apply(simplify2array(l_deaths), c(1,2), quantile, UI[1], na.rm = T)
  a_deaths_ub <- apply(simplify2array(l_deaths), c(1,2), quantile, UI[2], na.rm = T)
  
  # Format arrays in to data frames
  df_frac_lb <- as.data.frame(cbind(df_idcols, 
                                     Variable = rep('Fraction', nrow(df_idcols)),
                                     Quantile = rep('Lower', nrow(df_idcols)), 
                                     a_frac_lb))
  df_frac_ub <- as.data.frame(cbind(df_idcols, 
                                     Variable = rep('Fraction', nrow(df_idcols)),
                                     Quantile = rep('Lower', nrow(df_idcols)), 
                                     a_frac_ub))
  df_rates_lb <- as.data.frame(cbind(df_idcols, 
                                  Variable = rep('Rate', nrow(df_idcols)),
                                  Quantile = rep('Lower', nrow(df_idcols)), 
                                  a_rates_lb))
  df_rates_ub <- as.data.frame(cbind(df_idcols, 
                                     Variable = rep('Rate', nrow(df_idcols)),
                                     Quantile = rep('Lower', nrow(df_idcols)), 
                                     a_rates_ub))
  df_deaths_lb <- as.data.frame(cbind(df_idcols, 
                                     Variable = rep('Deaths', nrow(df_idcols)),
                                     Quantile = rep('Lower', nrow(df_idcols)), 
                                     a_deaths_lb))
  df_deaths_ub <- as.data.frame(cbind(df_idcols, 
                                     Variable = rep('Deaths', nrow(df_idcols)),
                                     Quantile = rep('Lower', nrow(df_idcols)), 
                                     a_deaths_ub))  
  # Rbind
  df_res <- rbind(df_frac_lb, df_frac_ub, df_rates_lb, df_rates_ub, df_deaths_lb, df_deaths_ub)
  
  return(df_res)
}

# mortDraws <- draws_csmf_ALL
# datAux <- mortDraws[[1]]
# 
# codAux <- codAll[codAll %in% names(datAux)]
# 
# rates <- datAux
# rates[, paste(codAux)] <- rates[, paste(codAux)] * rates[, 'Rate']
# frac <- datAux
# deaths <- datAux
# deaths[, paste(codAux)] <- deaths[, paste(codAux)] * deaths[, 'Deaths']
# 
# apply(rates, c(1, 2), quantile, UI[1], na.rm = T)
# test <- mortDraws[1:5]
# test <- lapply(test, function(x) as.matrix(x[codAux]))
# res <- apply(simplify2array(test), c(1,2), quantile, UI[1], na.rm = T)

# DRAWS_CSMF <- mortDraws[1:5]



##################################################
####
####   Format CSMFs, national
####
##################################################

fn_format_results <- function(dat, key_region, key_ctryclass, codAll){
  
  # Round deaths
  dat$Deaths <- round(dat$Deaths)
  # Round rate
  dat$Rate <- round(dat$Rate, 5)
  # Round fractions
  v_cod <- codAll[codAll %in% names(dat)]
  #v_cod <- names(dat)[!(names(dat) %in% c(idVars, "Deaths", "Rate"))]
  dat[,v_cod] <- round(dat[,v_cod], 5)
  
  # Add age group
  if(!("AgeLow" %in% names(dat))){
    dat$AgeLow <- ageLow
    dat$AgeUp <- ageUp
  }

  # Merge on regions
  dat <- merge(dat, key_region, by = "ISO3")

  # Merge on country class
  dat <- merge(dat, key_ctryclass[,c("ISO3", "Group2010", "FragileState")])
  names(dat)[names(dat) == "Group2010"] <- "Model"

  # Order columns
  dat <- dat[, c("ISO3", "Year", "AgeLow", "AgeUp", "Sex", "Model", "FragileState",
                 "WHOname", "SDGregion", "UNICEFReportRegion1", "UNICEFReportRegion2",
                  "Deaths", "Rate", v_cod)]
  
  # Tidy up
  dat <- dat[order(dat$ISO3, dat$Year, dat$Sex), ]
  rownames(dat) <- NULL
  
  return(dat)
}

##################################################
####
####   Calculate deaths, rates, csmfs for global regions; format
####
##################################################

fn_calc_region <- function(dat, codAll){
  
  # Create unified variable for region
  dat$Region <- dat$UNICEFReportRegion1
  # If report region 2 is not missing, use it instead
  dat$Region[which(dat$UNICEFReportRegion2 != "")] <- dat$UNICEFReportRegion2[which(dat$UNICEFReportRegion2 != "")]
  
  # Causes of death for this age group
  v_cod <- codAll[codAll %in% names(dat)]

  # Manually add extra regions
  df_world <- dat
  df_world$Region <- "World"
  df_eca <- subset(dat, UNICEFReportRegion1 == "Europe and central Asia")
  df_eca$Region <- "Europe and central Asia"
  df_ssa <- subset(dat, UNICEFReportRegion1 == "Sub-Saharan Africa")
  df_ssa$Region <- "Sub-Saharan Africa"
  dat <- rbind(dat, df_world, df_eca, df_ssa)
  
  # Create list of regions, move world to front
  v_regions <- sort(unique(dat$Region))
  v_regions <- v_regions[c(which(v_regions == "World"), which(v_regions != "World"))]
  
  # Convert CSMFs to deaths
  dat[, paste(v_cod)] <- dat[, paste(v_cod)] * dat$Deaths
  
  # Back calculate denominator from deaths and mortality rate
  dat$Px <- dat$Deaths/dat$Rate
  
  # Aggregate countries for each region
  dat <- ddply(dat, ~Region, function(x){aggregate(x[, c("Deaths", "Px", paste(v_cod))], 
                                                   by = list(x$Year, x$Sex, x$AgeLow, x$AgeUp), sum, na.rm = T)})
  names(dat)[1:5] <- c("Region", "Year", "Sex", "AgeLow", "AgeUp")
  
  # Re-calculate CSMFs
  dat[, paste(v_cod)] <- dat[, paste(v_cod)] / dat$Deaths
  dat[, paste(v_cod)] <- dat[, paste(v_cod)] / rowSums(dat[, paste(v_cod)])
  
  # Re-calculate mortality rate
  dat$Rate <- dat$Deaths / dat$Px
  # Remove denominator
  dat <- dat[, names(dat) != "Px"]
  
  # Order columns
  dat <- dat[, c("Region", "Year", "AgeLow", "AgeUp", "Sex", "Deaths", "Rate", paste(codAll[codAll %in% v_cod]))]
  
  # Tidy up
  dat$Region <- factor(dat$Region, levels = v_regions, ordered = TRUE)
  dat <- dat[order(dat$Region, dat$Year, dat$Sex), ]
  dat$Region <- as.character(dat$Region)
  rownames(dat) <- NULL
  
  return(dat)
  
}


##################################################
####
####   Calculate deaths, rates, csmfs for non-standard age intervals
####
##################################################

fn_calc_agg_rate <- function(ageLow, ageUp, env, dat05to09, dat10to14, dat15to19f, dat15to19m){
  
  v_keepcols <- c(idVars, codAll, "Rate", "Region")
  
  # Transform fractions into deaths
  if(ageLow == 5){
    #dat05to09 <- read.csv(paste("./gen/results/output/PointEstimates_National_05to09_", resDate, ".csv", sep =""))
    dat05to09[, codAll[codAll %in% names(dat05to09)]] <- dat05to09[, codAll[codAll %in% names(dat05to09)]] * dat05to09$Deaths
    dat05to09 <- dat05to09[,names(dat05to09) %in% v_keepcols]
    #dat10to14 <- read.csv(paste("./gen/results/output/PointEstimates_National_10to14_", resDate, ".csv", sep =""))
    dat10to14[, codAll[codAll %in% names(dat10to14)]] <- dat10to14[, codAll[codAll %in% names(dat10to14)]] * dat10to14$Deaths
    dat10to14 <- dat10to14[,names(dat10to14) %in% v_keepcols]
  }
  if(ageLow == 10){
    #dat10to14 <- read.csv(paste("./gen/results/output/PointEstimates_National_10to14_", resDate, ".csv", sep =""))
    dat10to14[, codAll[codAll %in% names(dat10to14)]] <- dat10to14[, codAll[codAll %in% names(dat10to14)]] * dat10to14$Deaths
    dat10to14 <- dat10to14[,names(dat10to14) %in% v_keepcols]
  }
  if(ageUp == 19){
    #dat15to19f <- read.csv(paste("./gen/results/output/PointEstimates_National_15to19f_", resDate, ".csv", sep =""))
    dat15to19f[, codAll[codAll %in% names(dat15to19f)]] <- dat15to19f[, codAll[codAll %in% names(dat15to19f)]] * dat15to19f$Deaths
    #dat15to19m <- read.csv(paste("./gen/results/output/PointEstimates_National_15to19m_", resDate, ".csv", sep =""))
    dat15to19m[, codAll[codAll %in% names(dat15to19m)]] <- dat15to19m[, codAll[codAll %in% names(dat15to19m)]] * dat15to19m$Deaths
    # Combine sexes
    dat15to19 <- dat15to19f
    dat15to19[, codAll[codAll %in% names(dat15to19)]] <- dat15to19[, codAll[codAll %in% names(dat15to19)]] + dat15to19m[, codAll[codAll %in% names(dat15to19m)]]
    dat15to19$Sex[dat15to19$Sex == sexLabels[2]] <- sexLabels[1]
    dat15to19 <- dat15to19[,names(dat15to19) %in% v_keepcols]
    # Get sex-combined rate from IGME crisis-included envelope
    #env <- read.csv("./gen/data-prep/output/env_crisisincl_u20.csv")
    env <- subset(env, Sex == sexLabels[1] & AgeLow == 15 & AgeUp == 19)[,c("ISO3", "Year", "Rate2")]
    names(env)[names(env) == "Rate2"] <- "Rate"
  }
  
  # Merge rates for different age groups
  if(ageLow == 5 & ageUp == 19){
    l_df <- list(dat05to09[, c("ISO3","Year","Rate")], dat10to14[, c("ISO3","Year","Rate")], env[, c("ISO3","Year","Rate")])
  }
  if(ageLow == 5 & ageUp == 14){
    l_df <- list(dat05to09[, c("ISO3","Year","Rate")], dat10to14[, c("ISO3","Year","Rate")])
  }
  if(ageLow == 10 & ageUp == 19){
    l_df <- list(dat10to14[, c("ISO3","Year","Rate")], env[, c("ISO3","Year","Rate")])
  }
  if(ageLow == 15 & ageUp == 19){
    l_df <- list(env)
  }
  if(length(l_df)>1){ 
    df_rate <- Reduce(function(x, y) merge(x, y, by = c("ISO3", "Year"), all=TRUE), l_df)
  }else{
    df_rate <- l_df[[1]]
  }
  names(df_rate)[names(df_rate) == "Rate.x"] <- "Rate1" 
  names(df_rate)[names(df_rate) == "Rate.y"] <- "Rate2"
  names(df_rate)[names(df_rate) == "Rate"] <- "Rate3"
  
  # Calculate aggregate rate
  if(ageUp - ageLow == 4){names(df_rate)[names(df_rate) == "Rate3"] <- "Rate"}
  if(ageUp - ageLow == 9){df_rate$Rate <- 1000 - (1000 - df_rate$Rate1)*(1 - df_rate$Rate2 / 1000)}
  if(ageUp - ageLow == 14){df_rate$Rate <- 1000 - (1000 - df_rate$Rate1)*(1 - df_rate$Rate2 / 1000)*(1 - df_rate$Rate3 / 1000)}
  df_rate <- df_rate[, c("ISO3", "Year", "Rate")]
  
  # Remove old Rate columns, add missing COD, rbind
  if(ageLow == 5 & ageUp == 19){
    dat05to09 <- dat05to09[, !names(dat05to09) %in% c("Rate")]
    dat10to14 <- dat10to14[, !names(dat10to14) %in% c("Rate")]
    dat15to19 <- dat15to19[, !names(dat15to19) %in% c("Rate")]
    addCOD <- codAll[which(!codAll %in% names(dat05to09))]
    dat05to09[, paste(addCOD)] <- 0
    addCOD <- codAll[which(!codAll %in% names(dat10to14))]
    dat10to14[, paste(addCOD)] <- 0
    addCOD <- codAll[which(!codAll %in% names(dat15to19))]
    dat15to19[, paste(addCOD)] <- 0
    dat <- rbind(dat05to09, dat10to14, dat15to19)
  }
  if(ageLow == 5 & ageUp == 14){
    dat05to09 <- dat05to09[, !names(dat05to09) %in% c("Rate")]
    dat10to14 <- dat10to14[, !names(dat10to14) %in% c("Rate")]
    addCOD <- names(dat10to14)[!(names(dat10to14) %in% names(dat05to09))]
    dat05to09[, paste(addCOD)] <- 0
    addCOD <- names(dat05to09)[!(names(dat05to09) %in% names(dat10to14))]
    dat10to14[, paste(addCOD)] <- 0
    dat <- rbind(dat05to09, dat10to14)
  }
  if(ageLow == 10 & ageUp == 19){
    dat10to14 <- dat10to14[, !names(dat10to14) %in% c("Rate")]
    dat15to19 <- dat15to19[, !names(dat15to19) %in% c("Rate")]
    addCOD <- names(dat15to19)[!(names(dat15to19) %in% names(dat10to14))]
    dat10to14[, paste(addCOD)] <- 0
    addCOD <- names(dat10to14)[!(names(dat10to14) %in% names(dat15to19))]
    dat15to19[, paste(addCOD)] <- 0
    dat <- rbind(dat10to14, dat15to19)
  }
  if(ageLow == 15 & ageUp == 19){
    dat15to19 <- dat15to19[, !names(dat15to19) %in% c("Rate")]
    dat <- dat15to19
  }
  
  # Aggregate ages
  dat <- aggregate(dat[, codAll[codAll %in% names(dat)]], by = list(dat$ISO3, dat$Year, dat$Sex), sum)
  names(dat)[1:3] <- idVars
  
  # Add missing variables
  dat$AgeLow <- ageLow
  dat$AgeUp <- ageUp
  dat$Deaths <- round(rowSums(dat[, codAll[codAll %in% names(dat)]]))
  dat <- merge(dat, df_rate, by = c("ISO3", "Year"), all.x = T, all.y = F)
  
  # Back transform into fractions
  dat[, codAll[codAll %in% names(dat)]] <- round(dat[, codAll[codAll %in% names(dat)]] / rowSums(dat[, codAll[codAll %in% names(dat)]]), 5)
  
  # Tidy up
  dat <- dat[order(dat$ISO3, dat$Year), ]
  rownames(dat) <- NULL
  
  return(dat)
}

##################################################
####
####   Visualization: compare CSMFs to results of previous year
####
##################################################

fn_compare_csmf <- function(dat, dat_Old, regional = FALSE, sample = NULL){
  
  if(regional == FALSE){
    dat_Old$name <- dat_Old$ISO3
    dat$name <- dat$ISO3
  }else{
    dat_Old$name <- dat_Old$Region
    dat$name <- dat$Region
  }

  if(sexSplit){
    dat_Old$Sex[dat_Old$Sex == "B"] <- sexLabels[1]
    dat_Old$Sex[dat_Old$Sex == "F"] <- sexLabels[2]
    dat_Old$Sex[dat_Old$Sex == "M"] <- sexLabels[3]
    dat_Old <- subset(dat_Old, Sex == sexLabel)
  }
  
  dat$update <- "new"
  dat_Old$update <- "old"

  dat <- bind_rows(dat, dat_Old)
  dat$update <- factor(dat$update, levels = c("old", "new"))
  
  # Delete unnecessary columns
  dat <- dat[-grep(c("ISO3|Region|FragileState|WHOname|SDGregion|UNICEFReportRegion1|UNICEFReportRegion2|Deaths|Rate|Qx|Model"), names(dat))]
  
  # Reshape mortality fractions into long format
  dat <- melt(setDT(dat), id.vars = c("update","name","AgeLow","AgeUp", "Sex","Year"))
  
  # Sample countries for national results
  if(length(sample) > 0){
    dat <- subset(dat, name %in% sample)
  }
  
  # Order plots alphabetically by world region and then nation
  
  plots <- dlply(dat, ~name,
                 function(x)
                   ggplot(data = x) + 
                   geom_line(aes(x=Year, y=value, color = update, linetype = update), linewidth = 1) +
                   labs(title = x$name, subtitle = paste(x$AgeLow,"-",x$AgeUp,", ", x$Sex, sep = "")) + 
                   xlab("") + ylab("") +
                   coord_cartesian(xlim = c(2000,2020), ylim = c(0,.8)) +
                   scale_x_continuous(breaks = c(2000, 2010, 2020)) +
                   scale_color_manual(values = c("gray", "firebrick2")) +
                   scale_linetype_manual(values = c("solid", "longdash")) +
                   facet_wrap(~variable, scales = "free") +
                   theme_classic() +
                   theme(panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank(),
                         strip.background = element_blank(),
                         panel.border = element_rect(colour = "black", fill = NA),
                         plot.subtitle = element_text(hjust = 0),
                         axis.text = element_text(size = 8)))
  mg <- marrangeGrob(grobs = plots, nrow=1, ncol=1, top = NULL)
  
  # Save output(s)
  ggsave(paste("./gen/results/audit/csmf_comparison_", ifelse(regional == FALSE, "national", "regional"), "_", ageGroup,"_", format(Sys.Date(), format="%Y%m%d"), ".pdf", sep=""), mg, height = 10, width = 8, units = "in")
  
}



