################################################################################
#' @description Format regional draws
#' @return ....
################################################################################
#' Libraries
library(data.table)
#' Inputs
source("./src/prepare-session/set-inputs.R")
source("./src/prepare-session/create-session-variables.R")
# List of regions from IGME
info <- read.csv("./data/igme/envelopes/regional/15-19/male/Rates & Deaths(ADJUSTED)_UNICEFReportRegion-males.csv") 
# Location of files
path <- "./data/igme/envelopes/regional/" 
# Draw file names
if(ageGroup == "05to09"){regDeaths  <- "5-9/UNICEFReportRegion_death0.all.rtj.rda"
                         regRates   <- "5-9/UNICEFReportRegion_imr.rtj.rda"
                         worldDeaths <- "5-9/death0.all.wtj.rda"
                         worldRates <-  "5-9/imr.wtj.rda"}
if(ageGroup == "10to14"){regDeaths   <- "10-14/UNICEFReportRegion_death1to4.all.rtj.rda"
                         regRates    <- "10-14/UNICEFReportRegion_cmr.rtj.rda"
                         worldDeaths <- "10-14/death1to4.all.wtj.rda"
                         worldRates  <- "10-14/cmr.wtj.rda" }
if(ageGroup == "15to19f"){regWom <- "15-19/female/Rates & Deaths(ADJUSTED)_UNICEFReportRegion-females.csv"}
if(ageGroup == "15to19m"){regMen  <- "15-19/male/Rates & Deaths(ADJUSTED)_UNICEFReportRegion-males.csv"}
###############################################################################

## Load region names
v_regions <- unique(info$Region)
v_regions <- v_regions[!(v_regions %in% "World")]

# This script selects columns from the draws which pertain to Years. 
# via the code "- (length(Years)-1):0"
# !!! Need to ensure that latest draw is same as highest value in Years.
warning("Ensure that latest Draw in IGME data is the same as latest year being predicted.")


# Load data ---------------------------------------------------------------

if(ageLow %in% 5){
  # Deaths
  load(paste0(path, regDeaths))
  regDea <- death0.all.rtj  
  rm(death0.all.rtj  )
  # Rates
  load(paste0(path, regRates))
  regQx <- imr.rtj
  rm(imr.rtj)
  # World deaths
  load(paste0(path, worldDeaths))
  worldDea <- death0.all.wtj[, , 1]
  rm(death0.all.wtj)
  # World rates
  load(paste0(path, worldRates))
  worldQx <- imr.wtj[, , 1]
  rm(imr.wtj)
}

if(ageLow %in% 10){
  # Deaths
  load(paste0(path, regDeaths))
  regDea <- death1to4.all.rtj  
  rm(death1to4.all.rtj)
  # Rates
  load(paste0(path, regRates))
  regQx <- cmr.rtj
  rm(cmr.rtj)
  # World deaths
  load(paste0(path, worldDeaths))
  worldDea <- death1to4.all.wtj[, , 1]
  rm(death1to4.all.wtj)
  # World rates
  load(paste0(path, worldRates))
  worldQx <- cmr.wtj[, , 1]
  rm(cmr.wtj)
}
if(ageGroup == "15to19f"){
  env_reg <- read.csv(paste0(path, regWom))
}
if(ageGroup == "15to19m"){
  env_reg <- read.csv(paste0(path, regMen))
}


# Reshape data ------------------------------------------------------------

if(ageLow %in% c(5, 10)){
  
  # Select years
  regDea <- as.matrix(regDea[, ncol(regDea) - (length(Years) - 1):0, 1])
  regQx <- as.matrix(regQx[, ncol(regQx) - (length(Years) - 1):0, 1])
  worldDea <- worldDea[length(worldDea) - (length(Years) - 1):0]
  worldQx <- worldQx[length(worldQx) - (length(Years) - 1):0]
  
  # Add column for regions
  regDea <- as.data.frame(cbind(v_regions, regDea))
  regQx <- as.data.frame(cbind(v_regions, regQx))
  
  # Add column names
  colnames(regDea) <- c('Region', Years)
  colnames(regQx) <- c('Region', Years)
  
  # Reshape to long
  regDea <- melt(setDT(regDea), id.vars = "Region", variable.name = "Year", value.name = "Deaths2")
  regQx <- melt(setDT(regQx), id.vars = "Region", variable.name = "Year", value.name = "Rate2")
  
  # Merge
  env_reg <- merge(regDea, regQx, by = c("Region", "Year"))
  
  # Rbind world deaths and rates
  env_reg <- rbind(env_reg, 
                   data.frame(Region = rep('World', length(Years)),
                             Year = Years,
                             Deaths2 = worldDea, Rate2 = worldQx))
}
if(ageLow == 15){
  # Select years
  env_reg <- env_reg[env_reg$Year %in% Years, ]
  # Select variables
  env_reg <- env_reg[, c("Region", "Year", "Deaths.age.15to19.median", "X5q15")]
  names(env_reg) <- c('Region', 'Year', 'Deaths2', 'Rate2')
}  

# Recode region names to match with key_region
env_reg$Region[env_reg$Region == "Eastern Europe and Central Asia"] <- "Eastern Europe and central Asia"
env_reg$Region[env_reg$Region == "Europe and Central Asia"] <- "Europe and central Asia" 
env_reg$Region[env_reg$Region == "West and Central Africa"] <- "West and central Africa"

# Save output(s) ----------------------------------------------------------

write.csv(env_reg, paste("./gen/data-management/output/env_",ageGroup,"REG.csv", sep = ""), row.names = FALSE)
