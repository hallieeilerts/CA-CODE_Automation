###################################################################
########################## BEGIN-INPUTS ###########################
###################################################################

# Load packages and session variables if not already loaded
if(!exists("sessionVars")){source("./src/prepare-session.R")
  load("./gen/data-prep/input/session-variables.Rdata")}

# Load input(s)

# List of countries
load('./data/igme/draws/info.rda')
# Location of crisis-free draws
pathCF <- './data/igme/draws/crisis-free/'
# Location of crisis-included draws
pathCI <- './data/igme/draws/crisis-included/'
# Draw file names
if(ageGroup == "05to09"){fileDeaths <- '5-9/death0.ctj.rda'
                         fileRates <- '5-9/imr.ctj.rda'}
if(ageGroup == "10to14"){fileDeaths <- '10-14/death1to4.ctj.rda'
                         fileRates <- '10-14/cmr.ctj.rda'}
if(ageGroup == "15to19f"){fileDeaths <- '15-19/female/death0.ctj.rda'
                          fileRates <- '15-19/female/imr.ctj.rda'}
if(ageGroup == "15to19m"){fileDeaths <- '15-19/male/death0.ctj.rda'
                          fileRates <- '15-19/male/imr.ctj.rda'}

# Classification keys
key_region <- read.csv("./gen/data-prep/output/key_region_u20.csv")
key_ctryclass <- read.csv("./gen/data-prep/output/key_ctryclass_u20.csv")

###################################################################
########################## END-INPUTS #############################
###################################################################

# Merge classification keys for region and country class
key_regclass <- merge(key_region, key_ctryclass[, c("ISO3", "Group2010", "FragileState")])

# This scripts selects columns from the draws which pertain to Years. 
# via the code "- (length(Years)-1):0"
# !!! Need to ensure that latest draw is same as highest value in Years.
warning("Ensure that latest Draw is the same as latest year being predicted.")

#--------------------#
# CRISIS-FREE DEATHS #
#--------------------#

# Draws location
load(paste0(pathCF, fileDeaths))

# Select years
if (ageLow %in% c(5, 15)) {
  deaths <- death0.ctj[, dim(death0.ctj)[2] - (length(Years)-1):0, ]
  deaths[1:5,1:22,1]
  rm(death0.ctj)
}
if (ageLow == 10) {
  deaths <- death1to4.ctj[, dim(death1to4.ctj)[2] - (length(Years)-1):0, ]
  rm(death1to4.ctj)
}
  
#------------------#
# ALL CAUSE DEATHS #
#------------------#

# Draws location
load(paste0(pathCI, fileDeaths))

# Select years
if (ageLow %in% c(5, 15)) {
  deathsAll <- death0.ctj[, dim(death0.ctj)[2] - (length(Years)-1):0, ]
  rm(death0.ctj)
}
if (ageLow == 10) {
  deathsAll <- death1to4.ctj[, dim(death1to4.ctj)[2] - (length(Years)-1):0, ]
  rm(death1to4.ctj)
}

#-----------------#
# ALL CAUSE RATES #
#-----------------#

# Draws location
load(paste0(pathCI, fileRates))

# Select years
if (ageLow %in% c(5, 15)) {
  ratesAll <- imr.ctj[, dim(imr.ctj)[2] - (length(Years)-1):0, ]
  rm(imr.ctj)
}
if (ageLow == 10) {
  ratesAll <- cmr.ctj[, dim(cmr.ctj)[2] - (length(Years)-1):0, ]
  rm(cmr.ctj)
}

# Country labels
dimnames(deaths) <- dimnames(deathsAll) <-
  dimnames(ratesAll) <- list(info$iso.c, Years, NULL)

# Select countries
deaths <- deaths[which(info$iso.c %in% key_regclass$ISO3), , ]
deathsAll <- deathsAll[which(info$iso.c %in% key_regclass$ISO3), , ]
ratesAll <- ratesAll[which(info$iso.c %in% key_regclass$ISO3), , ]

# Exclude draws with inconsistencies
dif <- deathsAll - deaths
idExclude <- c()
for (i in 1:dim(dif)[3]) {
  if (any(dif[,,i] < 0, na.rm = T)) idExclude <- c(idExclude, i)
}
if (length(idExclude) > 0) {
  deaths <- deaths[, , -idExclude]
  deathsAll <- deathsAll[, , -idExclude]
  ratesAll <- ratesAll[, , -idExclude]
}

# Format draws
draws_igme <- list(deaths = deaths, deathsAll = deathsAll, ratesAll = ratesAll)

#----------------#
#                #
# Quality checks #
#                #
#----------------#

# 1. Check for NAs

# 2. Check that crisis-free envelopes are not larger than crisis-included
# df_check <- dat
# df_check$ind1 <- ifelse(df_check$Deaths1 > df_check$Deaths2, 1, 0)
# if(sum(df_check$ind1) > 0){
#   stop("Crisis-free envelopes larger than crisis-included.")
# }
# rm(df_check)

# Remove unnecessary objects
rm(info, pathCF, pathCI, fileDeaths, fileRates, key_region, key_ctryclass, key_regclass)
# Remove objects created in this section
rm(dif, idExclude, deaths, deathsAll, ratesAll)

###################################################################
######################### BEGIN-OUTPUTS ###########################
###################################################################

# Save output(s)
saveRDS(draws_igme, file = paste("./gen/data-prep/output/draws_env_",ageGroup, ".rds",sep=""))
rm(draws_igme)

###################################################################
######################### END-OUTPUTS #############################
###################################################################
