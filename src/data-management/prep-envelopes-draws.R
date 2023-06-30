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
if(ageGroup == "15to19f"){fileDeaths <- "15-19/female/death0.ctj.rda"
                          fileRates  <- "15-19/female/imr.ctj.rda"}
if(ageGroup == "15to19m"){fileDeaths <- "15-19/male/death0.ctj.rda"
                          fileRates  <- "15-19/male/imr.ctj.rda"}
key_region    <- read.csv("./gen/data-management/output/key_region_u20.csv")
key_ctryclass <- read.csv("./gen/data-management/output/key_ctryclass_u20.csv")
################################################################################

# Merge classification keys for region and country class
key_regclass <- merge(key_region, key_ctryclass[, c("ISO3", "Group2010", "FragileState")])

# This script selects columns from the draws which pertain to Years. 
# via the code "- (length(Years)-1):0"
# !!! Need to ensure that latest draw is same as highest value in Years.
warning("Ensure that latest Draw is the same as latest year being predicted.")


# Crisis-free deaths ------------------------------------------------------

# Draws location
load(paste0(pathCF, fileDeaths))

# Select years
if (ageLow %in% c(5, 15)) {
  deaths1 <- death0.ctj[, dim(death0.ctj)[2] - (length(Years)-1):0, ]
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
  rm(imr.ctj)
}
if (ageLow == 10) {
  rates2 <- cmr.ctj[, dim(cmr.ctj)[2] - (length(Years)-1):0, ]
  rm(cmr.ctj)
}

# Country labels
dimnames(deaths1) <- dimnames(deaths2) <- dimnames(rates2) <- list(info$iso.c, Years, NULL)

# Select countries
deaths1 <- deaths1[which(info$iso.c %in% key_regclass$ISO3), , ]
deaths2 <- deaths2[which(info$iso.c %in% key_regclass$ISO3), , ]
rates2  <- rates2[which(info$iso.c %in% key_regclass$ISO3), , ]


# Quality check -----------------------------------------------------------

# Check that crisis-free envelopes are not larger than crisis-included

#------------------------#
# PATCH 2023.06.13
# Exclude draws with inconsistencies
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
# END PATCH
#------------------------#


# Crisis-free deaths ------------------------------------------------------

# Transform array into list of data frames
l_deaths1 <- lapply(1:dim(deaths1)[3], function(x){ as.data.frame(deaths1[, , x]) })
# Add ISO3 column to end of data frame
l_deaths1 <- lapply(l_deaths1, function(x){ x$ISO3 <- rownames(x) ; return(x)})
l_deaths1 <- lapply(l_deaths1, function(x){ rownames(x) <- NULL ; return(x)})
# Reshape to long
l_deaths1 <- lapply(l_deaths1, function(x){ cbind(x[ncol(x)], stack(x[-ncol(x)])) })
l_deaths1 <- lapply(l_deaths1, function(x){ names(x)[names(x) == "values"] <- "Deaths1" ; return(x)})
l_deaths1 <- lapply(l_deaths1, function(x){ names(x)[names(x) == "ind"]    <- idVars[2] ; return(x)})
l_deaths1 <- lapply(l_deaths1, function(x){ x$Sex <- sexLabel ; return(x)})

# Crisis-included deaths --------------------------------------------------

# Transform array into list of data frames
l_deaths2 <- lapply(1:dim(deaths2)[3], function(x){ as.data.frame(deaths2[, , x]) })
# Add ISO3 column to end of data frame
l_deaths2 <- lapply(l_deaths2, function(x){ x$ISO3 <- rownames(x) ; return(x)})
l_deaths2 <- lapply(l_deaths2, function(x){ rownames(x) <- NULL ; return(x)})
# Reshape to long
l_deaths2 <- lapply(l_deaths2, function(x){ cbind(x[ncol(x)], stack(x[-ncol(x)])) })
l_deaths2 <- lapply(l_deaths2, function(x){ names(x)[names(x) == "values"] <- "Deaths2" ; return(x)})
l_deaths2 <- lapply(l_deaths2, function(x){ names(x)[names(x) == "ind"]    <- idVars[2] ; return(x)})
l_deaths2 <- lapply(l_deaths2, function(x){ x$Sex <- sexLabel ; return(x)})

# Crisis-included rates --------------------------------------------------

# Transform array into list of data frames
l_rates2 <- lapply(1:dim(rates2)[3], function(x){ as.data.frame(rates2[, , x]) })
# Add ISO3 column to end of data frame
l_rates2 <- lapply(l_rates2, function(x){ x$ISO3 <- rownames(x) ; return(x)})
l_rates2 <- lapply(l_rates2, function(x){ rownames(x) <- NULL ; return(x)})
# Reshape to long
l_rates2 <- lapply(l_rates2, function(x){ cbind(x[ncol(x)], stack(x[-ncol(x)])) })
l_rates2 <- lapply(l_rates2, function(x){ names(x)[names(x) == "values"] <- "Rate" ; return(x)})
l_rates2 <- lapply(l_rates2, function(x){ names(x)[names(x) == "ind"]    <- idVars[2] ; return(x)})
l_rates2 <- lapply(l_rates2, function(x){ x$Sex <- sexLabel ; return(x)})


# Quality check -----------------------------------------------------------

# Check for NAs

#------------------------#
# PATCH 2023.06.09 
# Avoid NA on draws
l_deaths1 <- lapply(l_deaths1, function(x){ x$Deaths1[which(is.na(x$Deaths1))] <- 0 ; return(x)})
l_deaths2 <- lapply(l_deaths2, function(x){ x$Deaths2[which(is.na(x$Deaths2))] <- 0 ; return(x)})
l_rates2 <- lapply(l_rates2, function(x){ x$Rate[which(is.na(x$Rate))] <- 0 ; return(x)})
# END PATCH
#------------------------#

# Combine all
l_draws <- list(deaths1 = l_deaths1, deaths2 = l_deaths2, rates2 = l_rates2)

# Free up space before saving l_draws
rm(deaths1, deaths2, rates2, l_deaths1, l_deaths2, l_rates2)

# Save output(s) ----------------------------------------------------------

saveRDS(l_draws, file = paste("./gen/data-management/output/draws_env_", ageGroup, ".rds",sep=""))

# Remove unnecessary objects
rm(info, pathCF, pathCI, fileDeaths, fileRates, key_region, key_ctryclass, 
   key_regclass, dif, idExclude)
suppressWarnings(rm(i))
