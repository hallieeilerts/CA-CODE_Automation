###############################################################################
# Prepare session
###############################################################################

# Packages
library(readstata13) # Read Stata 13 databases

# Load session variables
load("./simple_update_2021/gen/data_preparation/input/session_variables.Rdata")
ls()

# These variables are passed by the make file
# If just running this script alone, need to be defined.
if(!exists("runmake")){
  Years <- 2000:2020
  ageLow <- 5
  ageUp <- ageLow + 4
  ageGroup <- paste0(ageLow, 'to', ageUp)
}

# Check for required variables
if(!(exists("idVars") & 
     exists("Years") & 
     exists("ageLow") & 
     exists("ageUp") & 
     exists("ageGroup"))){
    stop("Required variables are missing")
}

# China data
datCHN <- read.dta13('./data/simple_update_2021/china/20210330-ChinaDSP.dta', nonint.factors = T)

# IGME CRISIS-FREE (UPDATED) ENVELOPES
#datIGME <- read.csv('./data/simple_update_2021/igme/20220927-UN-IGME-RatesDeaths-crisis-free-1980-2021-allAges.csv')

# Causes-of-death RECLASSIFICATION
fileName <- paste0('./data/simple_update_2021/classification_keys/ReclassifiedCOD', ageGroup, '.csv')
reclass <- read.csv(fileName)

# -------------------------------------------------------------------------

# Select age group
if (ageLow == 5) {
  datCHN <- datCHN[datCHN$group == 'Both 5-9', ]
  datCHN$Sex <- 'B'
}
if (ageLow == 10) {
  datCHN <- datCHN[datCHN$group == 'Both 10-14', ]
  datCHN$Sex <- 'B'
}
if (ageLow == 15) {
  datCHN <- datCHN[datCHN$group %in% c('Female 15-19_(4)', 'Male 15-19'), ]
  datCHN$Sex <- 'F'
  datCHN$Sex[datCHN$group == 'Male 15-19'] <- 'M'
}

# RE-LABEL CAUSES OF DEATH
names(datCHN)[names(datCHN) == 'csdf3'] <- 'dia'
names(datCHN)[names(datCHN) == 'csdf4'] <- 'mea'
names(datCHN)[names(datCHN) == 'csdf7'] <- 'mening'
names(datCHN)[names(datCHN) == 'csdf9'] <- 'lri'
names(datCHN)[names(datCHN) == 'csdf10'] <- 'tb'
names(datCHN)[names(datCHN) == 'csdf11'] <- 'maternal'
names(datCHN)[names(datCHN) == 'csdf12'] <- 'othercd'
names(datCHN)[names(datCHN) == 'csdf14'] <- 'congen'
names(datCHN)[names(datCHN) == 'csdf15'] <- 'neoplasm'
names(datCHN)[names(datCHN) == 'csdf16'] <- 'cardio'
names(datCHN)[names(datCHN) == 'csdf17'] <- 'endo'
names(datCHN)[names(datCHN) == 'csdf18'] <- 'digest'
names(datCHN)[names(datCHN) == 'csdf19'] <- 'otherncd'
names(datCHN)[names(datCHN) == 'csdf21'] <- 'rta'
names(datCHN)[names(datCHN) == 'csdf22'] <- 'drown'
names(datCHN)[names(datCHN) == 'csdf23'] <- 'natdis'
names(datCHN)[names(datCHN) == 'csdf24'] <- 'intvio'
names(datCHN)[names(datCHN) == 'csdf25'] <- 'colvio'
names(datCHN)[names(datCHN) == 'csdf27'] <- 'selfharm'
names(datCHN)[names(datCHN) == 'csdf28'] <- 'otherinj'

# Add MISSING CATEGORIES to match VA COD list
if (!'hiv' %in% names(datCHN)) datCHN$hiv <- 0
if (!'mal' %in% names(datCHN)) datCHN$mal <- 0
if (!'typhoid' %in% names(datCHN)) datCHN$typhoid <- 0
if (!'other' %in% names(datCHN)) datCHN$other <- 0
if (!'undt' %in% names(datCHN)) datCHN$undt <- 0

# Re-classify causes of death
cod2 <- unique(reclass$Reclass)
for (i in 1:length(cod2)) {
  orig <- reclass$Original[reclass$Reclass == cod2[i]]
  if (length(orig) > 1) {
    datCHN[, paste(cod2[i])] <- apply(datCHN[, paste(orig)], 1, 
                                      function(x) {
                                        if (all(is.na(x))) {
                                          return(NA)
                                        } else return(sum(x, na.rm = T))
                                      })
  } else datCHN[, paste(cod2[i])] <- datCHN[, paste(orig)]
}

# Delete unnecessary columns (HIV will be incorporated later)
datCHN <- datCHN[, names(datCHN) %in% c('year', 'Sex', cod2)]
datCHN <- datCHN[, !names(datCHN) %in% c('HIV', 'Other', 'Undetermined')]
names(datCHN)[names(datCHN) == 'OtherCD'] <- 'OtherCMPN'

# Vector with Causes of death in China (to be used when estimating uncertainty)
codCHN <- names(datCHN)[-c(1:2)]

# Country and Year
datCHN$ISO3 <- 'CHN'
names(datCHN)[names(datCHN) == 'year'] <- idVars[2]


#----------------------#
# BEGIN NEW 2022.09.28 #

# APPLY ESTIMATES FROM 2019 TO 2020
dat2020 <- datCHN[datCHN$Year == 2019, ]
dat2020$Year <- 2020
datCHN <- rbind(datCHN, dat2020)
rm(dat2020)

# END NEW              #
#----------------------#


# # Add IGME envelopes
# datCHN <- merge(datCHN, y = datIGME, by = idVars, all.x = T)
# 
# # Check
# length(which(is.na(datCHN)))
# table(rowSums(datCHN[, !names(datCHN) %in% c(idVars, 'Deaths1', 'Deaths2', 'Qx')]))
# range(rowSums(datCHN[, !names(datCHN) %in% c(idVars, 'Deaths1', 'Deaths2', 'Qx')]))
# 
# # Adjust
# datCHN[, !names(datCHN) %in% c(idVars, 'Deaths1', 'Deaths2', 'Qx')] <-
#   datCHN[, !names(datCHN) %in% c(idVars, 'Deaths1', 'Deaths2', 'Qx')] /
#   rowSums(datCHN[, !names(datCHN) %in% c(idVars, 'Deaths1', 'Deaths2', 'Qx')])
# table(rowSums(datCHN[, !names(datCHN) %in% c(idVars, 'Deaths1', 'Deaths2', 'Qx')]))
# range(rowSums(datCHN[, !names(datCHN) %in% c(idVars, 'Deaths1', 'Deaths2', 'Qx')]))
# dim(datCHN)
# 
# # Tidy up
# datCHN <- datCHN[order(datCHN$ISO3, datCHN$Year, datCHN$Sex), ]
# rownames(datCHN) <- NULL
# head(datCHN)
# 
# # Remove unnecessary objects
# rm(reclass, idAdjust, cod2, orig)

write.csv(datCHN, "./gen/simple_update_2021/data_preparation/output/db_chinadspADJ.csv")

