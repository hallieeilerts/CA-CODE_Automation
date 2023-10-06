################################################################################
#' @description Format draws
#' @return List of length 3, corresponding to crisis-free deaths, crisis-included deaths, crisis-included rates.
#' Each first-level list element is a list of length number of draws.
#' Within first-level list element, each second-level list element is a data frame with c("ISO3", "Year", "Sex", Deaths1/Deaths2/Rate)
################################################################################
#' Libraries
#' Inputs
source("./src/prepare-session/set-inputs.R")
source("./src/prepare-session/create-session-variables.R")
load("./data/igme/draws/info.rda") # List of countries from IGME
pathCF <- "./data/igme/draws/crisis-free/" # Location of crisis-free draws
pathCI <- "./data/igme/draws/crisis-included/" # Location of crisis-included draws
# Draw file names
if(ageGroup == "05to09"){fileDeaths  <- "5-9/death0.ctj.rda"
                         fileRates   <- "5-9/imr.ctj.rda"}
if(ageGroup == "10to14"){fileDeaths  <- "10-14/death1to4.ctj.rda"
                         fileRates   <- "10-14/cmr.ctj.rda"}
if(ageGroup %in% c("15to19f","15to19m")){fileDeaths <- "15-19/both/death0.ctj.rda"
                                         fileRates  <- "15-19/both/imr.ctj.rda"
                                         fileDeathsWom <- "15-19/female/death0.ctj.rda"
                                         fileRatesWom  <- "15-19/female/imr.ctj.rda"
                                         fileDeathsMen <- "15-19/male/death0.ctj.rda"
                                         fileRatesMen  <- "15-19/male/imr.ctj.rda"
                                         envF <- read.csv(paste("./gen/data-management/output/env_15to19f.csv", sep=""))
                                         envM <- read.csv(paste("./gen/data-management/output/env_15to19m.csv", sep=""))}
key_ctryclass_u20 <- read.csv("./gen/data-management/output/key_ctryclass_u20.csv")
################################################################################

# This script selects columns from the draws which pertain to Years. 
# via the code "- (length(Years)-1):0"
# !!! Need to ensure that latest draw is same as highest value in Years.
warning("Ensure that latest Draw in IGME data is the same as latest year being predicted.")

# Crisis-free deaths ------------------------------------------------------

# Draws location
load(paste0(pathCF, fileDeaths))

# Select years
if (ageLow %in% c(5, 15)) {
  deaths1 <- death0.ctj[, dim(death0.ctj)[2] - (length(Years)-1):0, ]
  
  # If ageGroup is 15-19, perform action on females, males, and combined draws separately
  if(sexSplit){
    load(paste0(pathCF, fileDeathsMen))
    deaths1Men <- death0.ctj[, dim(death0.ctj)[2] - (length(Years)-1):0, ]
    load(paste0(pathCF, fileDeathsWom))
    deaths1Wom <- death0.ctj[, dim(death0.ctj)[2] - (length(Years)-1):0, ]
  }
  rm(death0.ctj)
}
if (ageLow == 10) {
  deaths1 <- death1to4.ctj[, dim(death1to4.ctj)[2] - (length(Years)-1):0, ]
  rm(death1to4.ctj)
}

# Crisis-included deaths --------------------------------------------------

# Draws location
load(paste0(pathCI, fileDeaths))

# Select years
if (ageLow %in% c(5, 15)) {
  deaths2 <- death0.ctj[, dim(death0.ctj)[2] - (length(Years)-1):0, ]
  
  # If ageGroup is 15-19, perform action on females, males, and combined draws separately
  if(sexSplit){
    load(paste0(pathCI, fileDeathsMen))
    deaths2Men <- death0.ctj[, dim(death0.ctj)[2] - (length(Years)-1):0, ]
    load(paste0(pathCI, fileDeathsWom))
    deaths2Wom <- death0.ctj[, dim(death0.ctj)[2] - (length(Years)-1):0, ]
  }
  rm(death0.ctj)
}
if (ageLow == 10) {
  deaths2 <- death1to4.ctj[, dim(death1to4.ctj)[2] - (length(Years)-1):0, ]
  rm(death1to4.ctj)
}

# Crisis-included rates ---------------------------------------------------

# Draws location
load(paste0(pathCI, fileRates))

# Select years
if (ageLow %in% c(5, 15)) {
  rates2 <- imr.ctj[, dim(imr.ctj)[2] - (length(Years)-1):0, ]
  
  # If ageGroup is 15-19, perform action on females, males, and combined draws separately
  if(sexSplit){
    load(paste0(pathCI, fileRatesMen))
    rates2Men <- imr.ctj[, dim(imr.ctj)[2] - (length(Years)-1):0, ]
    load(paste0(pathCI, fileRatesWom))
    rates2Wom <- imr.ctj[, dim(imr.ctj)[2] - (length(Years)-1):0, ]
  }
  rm(imr.ctj)
}
if (ageLow == 10) {
  rates2 <- cmr.ctj[, dim(cmr.ctj)[2] - (length(Years)-1):0, ]
  rm(cmr.ctj)
}

# Select countries --------------------------------------------------------

# Country labels
dimnames(deaths1) <- dimnames(deaths2) <- dimnames(rates2) <- list(info$iso.c, Years, NULL)
if(sexSplit){
  dimnames(deaths1Men) <- dimnames(deaths2Men) <- dimnames(rates2Men) <- list(info$iso.c, Years, NULL)
  dimnames(deaths1Wom) <- dimnames(deaths2Wom) <- dimnames(rates2Wom) <- list(info$iso.c, Years, NULL)
} 

# Select countries
deaths1 <- deaths1[which(info$iso.c %in% key_ctryclass_u20$ISO3), , ]
deaths2 <- deaths2[which(info$iso.c %in% key_ctryclass_u20$ISO3), , ]
rates2  <- rates2[which(info$iso.c %in% key_ctryclass_u20$ISO3), , ]
if(sexSplit){
  deaths1Men <- deaths1Men[which(info$iso.c %in% key_ctryclass_u20$ISO3), , ]
  deaths2Men <- deaths2Men[which(info$iso.c %in% key_ctryclass_u20$ISO3), , ]
  rates2Men <- rates2Men[which(info$iso.c %in% key_ctryclass_u20$ISO3), , ]  
  deaths1Wom <- deaths1Wom[which(info$iso.c %in% key_ctryclass_u20$ISO3), , ]
  deaths2Wom <- deaths2Wom[which(info$iso.c %in% key_ctryclass_u20$ISO3), , ]
  rates2Wom <- rates2Wom[which(info$iso.c %in% key_ctryclass_u20$ISO3), , ]  
}

# Patch: exclude draws with inconsistencies --------------------------------------

# Where crisis-free envelopes are larger than crisis-included 
dif <- deaths2 - deaths1
idExclude <- c()
for (i in 1:dim(dif)[3]) {
  if (any(dif[,,i] < 0, na.rm = T)) idExclude <- c(idExclude, i)
}
if (length(idExclude) > 0) {
  deaths1 <- deaths1[, , -idExclude]
  deaths2 <- deaths2[, , -idExclude]
  rates2  <- rates2[, , -idExclude]
}

if(sexSplit){
  
  dif <- deaths2Men - deaths1Men
  idExclude <- c()
  for (i in 1:dim(dif)[3]) {
    if (any(dif[,,i] < 0, na.rm = T)) idExclude <- c(idExclude, i)
  }
  if (length(idExclude) > 0) {
    deaths1Men <- deaths1Men[, , -idExclude]
    deaths2Men <- deaths2Men[, , -idExclude]
    rates2Men  <- rates2Men[, , -idExclude]
  }
  
  dif <- deaths2Wom - deaths1Wom
  idExclude <- c()
  for (i in 1:dim(dif)[3]) {
    if (any(dif[,,i] < 0, na.rm = T)) idExclude <- c(idExclude, i)
  }
  if (length(idExclude) > 0) {
    deaths1Wom <- deaths1Wom[, , -idExclude]
    deaths2Wom <- deaths2Wom[, , -idExclude]
    rates2Wom  <- rates2Wom[, , -idExclude]
  }
  
}

# Patch: use ratio of male:female 15-19 envelopes to fill in NAs in draws ---------

if(sexSplit){
  
  wom <- envF[, c("ISO3", "Year", "Deaths1", "Deaths2", "Rate2")]
  men <- envM[, c("ISO3", "Year", "Deaths1", "Deaths2", "Rate2")]
  
  # Ratios for deaths
  ratios <- merge(wom, men, by = c("ISO3", "Year"))
  ratios$Ratio1 <- ratios$Deaths1.x / (ratios$Deaths1.x + ratios$Deaths1.y)
  ratios$Ratio2 <- ratios$Deaths2.x / (ratios$Deaths2.x + ratios$Deaths2.y)
  
  # Ratios for probabilities
  ratioQ <- ratios$Rate2.x / ratios$Rate2.y
  ratios$RQwom <- (ratios$Deaths2.x + ratios$Deaths2.y * ratioQ) /
    (ratios$Deaths2.x + ratios$Deaths2.y)
  ratios$RQmen <- (ratios$Deaths2.x / ratioQ + ratios$Deaths2.y) /
    (ratios$Deaths2.x + ratios$Deaths2.y)
  
  # Split equally when few deaths
  thresh <- 1
  ratios$Ratio1[which(ratios$Deaths1.x < thresh | ratios$Deaths1.y < thresh)] <- .5
  ratios$Ratio2[which(ratios$Deaths2.x < thresh | ratios$Deaths2.y < thresh)] <- .5
  ratios$RQwom[which(ratios$Deaths2.x < thresh | ratios$Deaths2.y < thresh)] <- 1
  ratios$RQmen[which(ratios$Deaths2.x < thresh | ratios$Deaths2.y < thresh)] <- 1
  
  # Crisis deaths
  ratios$crisisWom <- F
  ratios$crisisWom[which(ratios$Deaths2.x - ratios$Deaths1.x > 0)] <- T
  ratios$crisisMen <- F
  ratios$crisisMen[which(ratios$Deaths2.y - ratios$Deaths1.y > 0)] <- T
  
  # Tidy up
  ratios <- ratios[, c("ISO3", "Year", "Ratio1", "Ratio2", 
                       "RQwom", "RQmen", "crisisWom", "crisisMen")]
  
  # For each country
  for (iso in 1:dim(deaths1Wom)[1]) {
    
    # For each year
    for (year in 1:dim(deaths1Wom)[2]) {
      
      # Threshold
      thresh2 <- 0
      
      # Proportions/Ratios
      ratio1 <- ratios$Ratio1[ratios$ISO3 == rownames(deaths1Wom)[iso] & ratios$Year == Years[year]]
      ratio2 <- ratios$Ratio2[ratios$ISO3 == rownames(deaths1Wom)[iso] & ratios$Year == Years[year]]
      rQwom <- ratios$RQwom[ratios$ISO3 == rownames(deaths1Wom)[iso] & ratios$Year == Years[year]]
      rQmen <- ratios$RQmen[ratios$ISO3 == rownames(deaths1Wom)[iso] & ratios$Year == Years[year]]
      
      # Female deaths (crisis-included)
      if (any(is.na(deaths2Wom[iso, year, ]))) {
        idna <- which(is.na(deaths2Wom[iso, year, ]))
        if (length(idna) < thresh2) {
          deaths2Wom[iso, year, idna] <- 
            round(rnorm(n = length(idna),
                        mean = mean(deaths2Wom[iso, year, ], na.rm = T),
                        sd = sd(deaths2Wom[iso, year, ], na.rm = T)))
        } else deaths2Wom[iso, year, idna] <- round(deaths2[iso, year, idna] * ratio2)
      }
      
      # Female deaths (crisis-free)
      if (any(is.na(deaths1Wom[iso, year, ]))) {
        # Crisis deaths on that year?
        if (ratios$crisisWom[ratios$ISO3 == rownames(deaths1Wom)[iso] & ratios$Year == Years[year]]) {
          idna <- which(is.na(deaths1Wom[iso, year, ]))
          # If below threshold, assign values randomly
          if (length(idna) < thresh2) {
            deaths1Wom[iso, year, idna] <- 
              round(rnorm(n = length(idna),
                          mean = mean(deaths1Wom[iso, year, ], na.rm = T),
                          sd = sd(deaths1Wom[iso, year, ], na.rm = T)))
          } else deaths1Wom[iso, year, idna] <- round(deaths1[iso, year, idna] * ratio1)
        } else deaths1Wom[iso, year, idna] <- deaths2Wom[iso, year, idna]
      }
      
      # Male deaths (crisis-included)
      if (any(is.na(deaths2Men[iso, year, ]))) {
        idna <- which(is.na(deaths2Men[iso, year, ]))
        if (length(idna) < thresh2) {
          deaths2Men[iso, year, idna] <- 
            round(rnorm(n = length(idna),
                        mean = mean(deaths2Men[iso, year, ], na.rm = T),
                        sd = sd(deaths2Men[iso, year, ], na.rm = T)))
        } else deaths2Men[iso, year, idna] <- round(deaths2[iso, year, idna] * (1 - ratio2))
      }
      
      # Male deaths (crisis-free)
      if (any(is.na(deaths1Men[iso, year, ]))) {
        # Crisis deaths on that year?
        if (ratios$crisisMen[ratios$ISO3 == rownames(deaths1Wom)[iso] & ratios$Year == Years[year]]) {
          idna <- which(is.na(deaths1Men[iso, year, ]))
          # If below threshold, assign values randomly
          if (length(idna) < thresh2) {
            deaths1Men[iso, year, idna] <- 
              round(rnorm(n = length(idna),
                          mean = mean(deaths1Men[iso, year, ], na.rm = T),
                          sd = sd(deaths1Men[iso, year, ], na.rm = T)))
          } else deaths1Men[iso, year, idna] <- round(deaths1[iso, year, idna] * (1 - ratio1))
        } else deaths1Men[iso, year, idna] <- deaths2Men[iso, year, idna]
      }
      
      # Female rates
      if (any(is.na(rates2Wom[iso, year, ]))) {
        idna <- which(is.na(rates2Wom[iso, year, ]))
        rates2[iso, year, idna] <- rates2[iso, year, idna] * rQwom
      }
      
      # Male rates
      if (any(is.na(rates2Men[iso, year, ]))) {
        idna <- which(is.na(rates2Men[iso, year, ]))
        rates2Men[iso, year, idna] <- rates2[iso, year, idna] * rQmen
      }
      
    }
    
  }
  rm(iso, men, wom, ratios, ratioQ, ratio1, ratio2, rQmen, rQwom, year)
  
  # Keep only one sex
  #if(ageGroup == "15to19m"){
  #  deaths1 <- deaths1Men
  #  deaths2 <- deaths2Men
  #  rates2  <- rates2Men
  #}
  #rm(deaths1Men, deaths2Men, rates2Men)
  
}

# Transform arrays into lists of data frames ---------------------------------

# Crisis-free deaths
# Transform array into list of data frames
l_deaths1 <- lapply(1:dim(deaths1)[3], function(x){ as.data.frame(deaths1[, , x]) })
# Add ISO3 column to end of data frame
l_deaths1 <- lapply(l_deaths1, function(x){ x$ISO3 <- rownames(x) ; return(x)})
l_deaths1 <- lapply(l_deaths1, function(x){ rownames(x) <- NULL ; return(x)})
# Reshape to long
l_deaths1 <- lapply(l_deaths1, function(x){ cbind(x[ncol(x)], stack(x[-ncol(x)])) })
l_deaths1 <- lapply(l_deaths1, function(x){ names(x)[names(x) == "values"] <- "Deaths1" ; return(x)})
l_deaths1 <- lapply(l_deaths1, function(x){ names(x)[names(x) == "ind"]    <- idVars[2] ; return(x)})
l_deaths1 <- lapply(l_deaths1, function(x){ x$Sex <- sexLabels[1] ; return(x)})
# If sex split, also perform actions on sex-specific draws
if(sexSplit){
  l_deaths1Men <- lapply(1:dim(deaths1Men)[3], function(x){ as.data.frame(deaths1Men[, , x]) })
  l_deaths1Men <- lapply(l_deaths1Men, function(x){ x$ISO3 <- rownames(x) ; return(x)})
  l_deaths1Men <- lapply(l_deaths1Men, function(x){ rownames(x) <- NULL ; return(x)})
  l_deaths1Men <- lapply(l_deaths1Men, function(x){ cbind(x[ncol(x)], stack(x[-ncol(x)])) })
  l_deaths1Men <- lapply(l_deaths1Men, function(x){ names(x)[names(x) == "values"] <- "Deaths1" ; return(x)})
  l_deaths1Men <- lapply(l_deaths1Men, function(x){ names(x)[names(x) == "ind"]    <- idVars[2] ; return(x)})
  l_deaths1Men <- lapply(l_deaths1Men, function(x){ x$Sex <- sexLabels[3] ; return(x)})
  l_deaths1Wom <- lapply(1:dim(deaths1Wom)[3], function(x){ as.data.frame(deaths1Wom[, , x]) })
  l_deaths1Wom <- lapply(l_deaths1Wom, function(x){ x$ISO3 <- rownames(x) ; return(x)})
  l_deaths1Wom <- lapply(l_deaths1Wom, function(x){ rownames(x) <- NULL ; return(x)})
  l_deaths1Wom <- lapply(l_deaths1Wom, function(x){ cbind(x[ncol(x)], stack(x[-ncol(x)])) })
  l_deaths1Wom <- lapply(l_deaths1Wom, function(x){ names(x)[names(x) == "values"] <- "Deaths1" ; return(x)})
  l_deaths1Wom <- lapply(l_deaths1Wom, function(x){ names(x)[names(x) == "ind"]    <- idVars[2] ; return(x)})
  l_deaths1Wom <- lapply(l_deaths1Wom, function(x){ x$Sex <- sexLabels[2] ; return(x)})
}

# Crisis-included deaths
# Transform array into list of data frames
l_deaths2 <- lapply(1:dim(deaths2)[3], function(x){ as.data.frame(deaths2[, , x]) })
# Add ISO3 column to end of data frame
l_deaths2 <- lapply(l_deaths2, function(x){ x$ISO3 <- rownames(x) ; return(x)})
l_deaths2 <- lapply(l_deaths2, function(x){ rownames(x) <- NULL ; return(x)})
# Reshape to long
l_deaths2 <- lapply(l_deaths2, function(x){ cbind(x[ncol(x)], stack(x[-ncol(x)])) })
l_deaths2 <- lapply(l_deaths2, function(x){ names(x)[names(x) == "values"] <- "Deaths2" ; return(x)})
l_deaths2 <- lapply(l_deaths2, function(x){ names(x)[names(x) == "ind"]    <- idVars[2] ; return(x)})
l_deaths2 <- lapply(l_deaths2, function(x){ x$Sex <- sexLabels[1] ; return(x)})
# If sex split, also perform actions on sex-specific draws
if(sexSplit){
  l_deaths2Men <- lapply(1:dim(deaths2Men)[3], function(x){ as.data.frame(deaths2Men[, , x]) })
  l_deaths2Men <- lapply(l_deaths2Men, function(x){ x$ISO3 <- rownames(x) ; return(x)})
  l_deaths2Men <- lapply(l_deaths2Men, function(x){ rownames(x) <- NULL ; return(x)})
  l_deaths2Men <- lapply(l_deaths2Men, function(x){ cbind(x[ncol(x)], stack(x[-ncol(x)])) })
  l_deaths2Men <- lapply(l_deaths2Men, function(x){ names(x)[names(x) == "values"] <- "Deaths2" ; return(x)})
  l_deaths2Men <- lapply(l_deaths2Men, function(x){ names(x)[names(x) == "ind"]    <- idVars[2] ; return(x)})
  l_deaths2Men <- lapply(l_deaths2Men, function(x){ x$Sex <- sexLabels[3] ; return(x)})
  l_deaths2Wom <- lapply(1:dim(deaths2Wom)[3], function(x){ as.data.frame(deaths2Wom[, , x]) })
  l_deaths2Wom <- lapply(l_deaths2Wom, function(x){ x$ISO3 <- rownames(x) ; return(x)})
  l_deaths2Wom <- lapply(l_deaths2Wom, function(x){ rownames(x) <- NULL ; return(x)})
  l_deaths2Wom <- lapply(l_deaths2Wom, function(x){ cbind(x[ncol(x)], stack(x[-ncol(x)])) })
  l_deaths2Wom <- lapply(l_deaths2Wom, function(x){ names(x)[names(x) == "values"] <- "Deaths2" ; return(x)})
  l_deaths2Wom <- lapply(l_deaths2Wom, function(x){ names(x)[names(x) == "ind"]    <- idVars[2] ; return(x)})
  l_deaths2Wom <- lapply(l_deaths2Wom, function(x){ x$Sex <- sexLabels[2] ; return(x)})
}

# Crisis-included rates
# Transform array into list of data frames
l_rates2 <- lapply(1:dim(rates2)[3], function(x){ as.data.frame(rates2[, , x]) })
# Add ISO3 column to end of data frame
l_rates2 <- lapply(l_rates2, function(x){ x$ISO3 <- rownames(x) ; return(x)})
l_rates2 <- lapply(l_rates2, function(x){ rownames(x) <- NULL ; return(x)})
# Reshape to long
l_rates2 <- lapply(l_rates2, function(x){ cbind(x[ncol(x)], stack(x[-ncol(x)])) })
l_rates2 <- lapply(l_rates2, function(x){ names(x)[names(x) == "values"] <- "Rate2" ; return(x)})
l_rates2 <- lapply(l_rates2, function(x){ names(x)[names(x) == "ind"]    <- idVars[2] ; return(x)})
l_rates2 <- lapply(l_rates2, function(x){ x$Sex <- sexLabels[1] ; return(x)})
# If sex split, also perform actions on sex-specific draws
if(sexSplit){
  l_rates2Men <- lapply(1:dim(rates2Men)[3], function(x){ as.data.frame(rates2Men[, , x]) })
  l_rates2Men <- lapply(l_rates2Men, function(x){ x$ISO3 <- rownames(x) ; return(x)})
  l_rates2Men <- lapply(l_rates2Men, function(x){ rownames(x) <- NULL ; return(x)})
  l_rates2Men <- lapply(l_rates2Men, function(x){ cbind(x[ncol(x)], stack(x[-ncol(x)])) })
  l_rates2Men <- lapply(l_rates2Men, function(x){ names(x)[names(x) == "values"] <- "Rate2" ; return(x)})
  l_rates2Men <- lapply(l_rates2Men, function(x){ names(x)[names(x) == "ind"]    <- idVars[2] ; return(x)})
  l_rates2Men <- lapply(l_rates2Men, function(x){ x$Sex <- sexLabels[3] ; return(x)})
  l_rates2Wom <- lapply(1:dim(rates2Wom)[3], function(x){ as.data.frame(rates2Wom[, , x]) })
  l_rates2Wom <- lapply(l_rates2Wom, function(x){ x$ISO3 <- rownames(x) ; return(x)})
  l_rates2Wom <- lapply(l_rates2Wom, function(x){ rownames(x) <- NULL ; return(x)})
  l_rates2Wom <- lapply(l_rates2Wom, function(x){ cbind(x[ncol(x)], stack(x[-ncol(x)])) })
  l_rates2Wom <- lapply(l_rates2Wom, function(x){ names(x)[names(x) == "values"] <- "Rate2" ; return(x)})
  l_rates2Wom <- lapply(l_rates2Wom, function(x){ names(x)[names(x) == "ind"]    <- idVars[2] ; return(x)})
  l_rates2Wom <- lapply(l_rates2Wom, function(x){ x$Sex <- sexLabels[2] ; return(x)})
}

# Quality check -----------------------------------------------------------

# Check for NAs

#------------------------#
# PATCH 2023.06.09 
# Avoid NA on draws
l_deaths1 <- lapply(l_deaths1, function(x){ x$Deaths1[which(is.na(x$Deaths1))] <- 0 ; return(x)})
l_deaths2 <- lapply(l_deaths2, function(x){ x$Deaths2[which(is.na(x$Deaths2))] <- 0 ; return(x)})
l_rates2 <- lapply(l_rates2, function(x){ x$Rate[which(is.na(x$Rate))] <- 0 ; return(x)})
if(sexSplit){
  l_deaths1Men <- lapply(l_deaths1Men, function(x){ x$Deaths1[which(is.na(x$Deaths1))] <- 0 ; return(x)})
  l_deaths2Men <- lapply(l_deaths2Men, function(x){ x$Deaths2[which(is.na(x$Deaths2))] <- 0 ; return(x)})
  l_rates2Men <- lapply(l_rates2Men, function(x){ x$Rate[which(is.na(x$Rate))] <- 0 ; return(x)})
  l_deaths1Wom <- lapply(l_deaths1Wom, function(x){ x$Deaths1[which(is.na(x$Deaths1))] <- 0 ; return(x)})
  l_deaths2Wom <- lapply(l_deaths2Wom, function(x){ x$Deaths2[which(is.na(x$Deaths2))] <- 0 ; return(x)})
  l_rates2Wom <- lapply(l_rates2Wom, function(x){ x$Rate[which(is.na(x$Rate))] <- 0 ; return(x)})
}
# END PATCH
#------------------------#

# Combine all
envDraws <- list(deaths1 = l_deaths1, deaths2 = l_deaths2, rates2 = l_rates2)
if(sexSplit){
  envDraws_15to19m <- list(deaths1 = l_deaths1Men, deaths2 = l_deaths2Men, rates2 = l_rates2Men)
  envDraws_15to19f <- list(deaths1 = l_deaths1Wom, deaths2 = l_deaths2Wom, rates2 = l_rates2Wom)
}

# Free up space before saving l_draws
suppressWarnings(rm(deaths1, deaths2, rates2, l_deaths1, l_deaths2, l_rates2,
                    deaths1Men, deaths2Men, rates2Men, l_deaths1Men, l_deaths2Men, l_rates2Men,
                    deaths1Wom, deaths2Wom, rates2Wom, l_deaths1Wom, l_deaths2Wom, l_rates2Wom))

# Check that all expected countries are included --------------------------

if(sum(!(unique(key_ctryclass_u20$ISO3) %in% info$iso.c)) > 0){
  stop("Required countries missing from data input.")
}

if(sum(!(unique(key_ctryclass_u20$ISO3) %in% envDraws$deaths1[[1]]$ISO3)) > 0){
  stop("Required countries missing from formatted envelopes.")
}

# Save output(s) ----------------------------------------------------------

if(!sexSplit){
  saveRDS(envDraws, file = paste("./gen/data-management/output/envDraws_", ageGroup, ".rds",sep=""))
}else{
  saveRDS(envDraws, file = paste("./gen/data-management/output/envDraws_15to19.rds",sep=""))
  saveRDS(envDraws_15to19m, file = paste("./gen/data-management/output/envDraws_15to19m.rds",sep=""))
  saveRDS(envDraws_15to19f, file = paste("./gen/data-management/output/envDraws_15to19f.rds",sep=""))
}
