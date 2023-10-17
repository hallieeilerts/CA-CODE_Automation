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
if(ageGroup %in% c("15to19f","15to19m")){regf <- "15-19/female/Rates & Deaths(ADJUSTED)_UNICEFReportRegion-females.csv"
                                         regm  <- "15-19/male/Rates & Deaths(ADJUSTED)_UNICEFReportRegion-males.csv"}

# Classification keys
key_region_u20    <- read.csv("./gen/data-management/output/key_region_u20.csv")
###############################################################################

## Load region names from info file provided by IGME
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
if(ageLow %in% 15){
  env_15to19mREG <- read.csv(paste0(path, regm))
  env_15to19fREG <- read.csv(paste0(path, regf))
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
  env_REG <- merge(regDea, regQx, by = c("Region", "Year"))
  
  # Rbind world deaths and rates
  env_REG <- rbind(env_REG, 
                   data.frame(Region = rep('World', length(Years)),
                             Year = Years,
                             Deaths2 = worldDea, Rate2 = worldQx))
}
if(ageLow == 15){
  
  # Select years and variables
  env_15to19mREG <- env_15to19mREG[env_15to19mREG$Year %in% Years, ]
  env_15to19mREG <- env_15to19mREG[, c("Region", "Year", "Deaths.age.15to19.median", "X5q15")]
  names(env_15to19mREG) <- c('Region', 'Year', 'Deaths2', 'Rate2')
  
  # Select years and variables
  env_15to19fREG <- env_15to19fREG[env_15to19fREG$Year %in% Years, ]
  env_15to19fREG <- env_15to19fREG[, c("Region", "Year", "Deaths.age.15to19.median", "X5q15")]
  names(env_15to19fREG) <- c('Region', 'Year', 'Deaths2', 'Rate2')
  
  # Combine sexes
  # Back calculate px
  env_15to19fREG$Px <- env_15to19fREG$Deaths2/env_15to19fREG$Rate2
  env_15to19mREG$Px <- env_15to19mREG$Deaths2/env_15to19mREG$Rate2
  env_REG <- merge(env_15to19fREG, env_15to19mREG, by = c("Region", "Year"), suffixes = c(".f", ".m"))
  # Sum deaths, recalculate rate
  env_REG$Deaths2 <- env_REG$Deaths2.f + env_REG$Deaths2.m
  env_REG$Rate2 <- env_REG$Deaths2/(env_REG$Px.f + env_REG$Px.m)
  
  # Only keep columns of interest
  env_15to19fREG <- env_15to19fREG[, c('Region', 'Year', 'Deaths2', 'Rate2')]
  env_15to19mREG <- env_15to19mREG[, c('Region', 'Year', 'Deaths2', 'Rate2')]
  env_REG <- env_REG[, c('Region', 'Year', 'Deaths2', 'Rate2')]
}  

key_region <- key_region_u20
key_region$region_lower <- tolower(key_region$Region)
key_region <- key_region[, c("Region", "region_lower")]
key_region <- key_region[!duplicated(key_region),]

# Recode region names to match with official IGME Region names in key_region
env_REG$region_lower <- tolower(env_REG$Region)
env_REG <- merge(env_REG, key_region, by = "region_lower", all.x = TRUE)
env_REG$recode_region <- ifelse(env_REG$Region.x != "World" & env_REG$Region.x != env_REG$Region.y, 1, 0)
# If the region name in the regional envelopes does not match the case of official region names, recode
env_REG$Region.x[env_REG$recode_region == 1] <- env_REG$Region.y[env_REG$recode_region == 1]

# Tidy up
names(env_REG)[names(env_REG) == "Region.x"] <- "Region"
env_REG <- env_REG[,c("Region", "Year", "Deaths2", "Rate2")]

if(ageLow == 15){
  # Recode region names to match with official IGME Region names in key_region
  env_15to19fREG$region_lower <- tolower(env_15to19fREG$Region)
  env_15to19fREG <- merge(env_15to19fREG, key_region, by = "region_lower", all.x = TRUE)
  env_15to19fREG$recode_region <- ifelse(env_15to19fREG$Region.x != "World" & env_15to19fREG$Region.x != env_15to19fREG$Region.y, 1, 0)
  # If the region name in the regional envelopes does not match the case of official region names, recode
  env_15to19fREG$Region.x[env_15to19fREG$recode_region == 1] <- env_15to19fREG$Region.y[env_15to19fREG$recode_region == 1]
  # Tidy up
  names(env_15to19fREG)[names(env_15to19fREG) == "Region.x"] <- "Region"
  env_15to19fREG <- env_15to19fREG[,c("Region", "Year", "Deaths2", "Rate2")]
  
  # Recode region names to match with official IGME Region names in key_region
  env_15to19mREG$region_lower <- tolower(env_15to19mREG$Region)
  env_15to19mREG <- merge(env_15to19mREG, key_region, by = "region_lower", all.x = TRUE)
  env_15to19mREG$recode_region <- ifelse(env_15to19mREG$Region.x != "World" & env_15to19mREG$Region.x != env_15to19mREG$Region.y, 1, 0)
  # If the region name in the regional envelopes does not match the case of official region names, recode
  env_15to19mREG$Region.x[env_15to19mREG$recode_region == 1] <- env_15to19mREG$Region.y[env_15to19mREG$recode_region == 1]
  # Tidy up
  names(env_15to19mREG)[names(env_15to19mREG) == "Region.x"] <- "Region"
  env_15to19mREG <- env_15to19mREG[,c("Region", "Year", "Deaths2", "Rate2")]
}


# Save output(s) ----------------------------------------------------------

if(ageLow != 15){
  write.csv(env_REG, paste("./gen/data-management/output/env_",ageGroup,"REG.csv", sep = ""), row.names = FALSE)
}else{
  write.csv(env_REG, paste("./gen/data-management/output/env_15to19REG.csv", sep = ""), row.names = FALSE)
  write.csv(env_15to19mREG, paste("./gen/data-management/output/env_15to19mREG.csv", sep = ""), row.names = FALSE)
  write.csv(env_15to19fREG, paste("./gen/data-management/output/env_15to19fREG.csv", sep = ""), row.names = FALSE)
}



