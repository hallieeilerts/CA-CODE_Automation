

#----------------------#
# IGME POINT ESTIMATES #
#----------------------#

#----------------------#
# BEGIN NEW 2022.09.28 #

# IGME CRISIS-FREE (UPDATED) ENVELOPES
datIGME <- read.csv('IGME-Envelopes/20220927-UN-IGME-RatesDeaths-crisis-free-1980-2021-allAges.csv')

# Tidy up envelopes: Deaths
n <- ifelse(ageLow == 5, 11, 13)
colKeep <- which(substr(names(datIGME), 1, n) == paste0('Deaths.', ageGroup))
datIGME <- cbind(datIGME[, c(1, 2, colKeep)])
datIGME <- as.data.frame(pivot_longer(datIGME, cols = 3:ncol(datIGME), names_to = 'Year', 
                                      values_to = paste0('dea', ageGroup)))
datIGME$Year <- as.numeric(substr(datIGME$Year, nchar(datIGME$Year) - 3, nchar(datIGME$Year)))

# Crisis-free data frame
datIGME <- datIGME[, -1]
names(datIGME)[1] <- 'ISO3'

# IGME ALL-CAUSE (UPDATED) ENVELOPES
datIGME2 <- read.csv('IGME-Envelopes/20220927-UN-IGME-RatesDeaths-1980-2021-allAges.csv')

# RATES 5 TO 19
colKeep <- which(substr(names(datIGME2), 1, 7) == 'MR5to14')
dat1 <- cbind(datIGME2[, 1:2], datIGME2[, colKeep])
dat1 <- as.data.frame(pivot_longer(dat1, cols = 3:ncol(dat1), 
                                   names_to = 'Year', values_to = 'q5to14'))
dat1$Year <- as.numeric(substr(dat1$Year, nchar(dat1$Year) - 3, nchar(dat1$Year)))
colKeep <- which(substr(names(datIGME2), 1, 8) == 'MR15to19')
dat2 <- cbind(datIGME2[, 1:2], datIGME2[, colKeep])
dat2 <- as.data.frame(pivot_longer(dat2, cols = 3:ncol(dat2), names_to = 'Year', 
                                   values_to = 'q15to19'))
dat2$Year <- as.numeric(substr(dat2$Year, nchar(dat2$Year) - 3, nchar(dat2$Year)))
dat5to19 <- merge(dat1, dat2, by = 1:3)
dat5to19 <- dat5to19[, -1]
names(dat5to19)[1] <- 'ISO3'
dat5to19 <- dat5to19[dat5to19$Year %in% Years, ]
dat5to19$q5to19 <- (1 - (1 - dat5to19$q5to14 / 1000)*(1 - dat5to19$q15to19 / 1000))*1000 
rownames(dat5to19) <- NULL
rm(colKeep, dat1, dat2)

# Tidy up envelopes: Rates
n <- ifelse(ageLow == 5, 6, 8)
colKeep <- which(substr(names(datIGME2), 1, n) == paste0('MR', ageGroup))
dat1 <- cbind(datIGME2[, 1:2], datIGME2[, colKeep])
dat1 <- as.data.frame(pivot_longer(dat1, cols = 3:ncol(dat1), names_to = 'Year', 
                                   values_to = paste0('q', ageGroup)))
dat1$Year <- as.numeric(substr(dat1$Year, nchar(dat1$Year) - 3, nchar(dat1$Year)))

# Tidy up envelopes: Deaths
n <- ifelse(ageLow == 5, 11, 13)
colKeep <- which(substr(names(datIGME2), 1, n) == paste0('Deaths.', ageGroup))
dat2 <- cbind(datIGME2[, 1:2], datIGME2[, colKeep])
dat2 <- as.data.frame(pivot_longer(dat2, cols = 3:ncol(dat2), names_to = 'Year', 
                                   values_to = paste0('dea', ageGroup)))
dat2$Year <- as.numeric(substr(dat2$Year, nchar(dat2$Year) - 3, nchar(dat2$Year)))

# All-cause data frame
datIGME2 <- merge(dat1, dat2, by = 1:3)
datIGME2 <- datIGME2[, -1]
names(datIGME2)[1] <- 'ISO3'
rm(colKeep, dat1, dat2, n)

# END NEW              #
#----------------------#


# Years of interest
datIGME <- datIGME[datIGME$Year %in% Years, ]
datIGME2 <- datIGME2[datIGME2$Year %in% Years, ]

# Variables of interest
if (sexSplit) {
  
  # Crisis free envelopes
  datIGME <- datIGME[, c('ISO3', 'Year', paste0('dea', ageLow, 'to', ageLow + 4, 'F'),
                         paste0('dea', ageLow, 'to', ageLow + 4, 'M'))]
  names(datIGME)[3:4] <- c('F', 'M')
  datIGME <- as.data.frame(pivot_longer(data = datIGME, cols = c('F', 'M'), 
                                        names_to = 'Sex', values_to = 'Deaths1'))
  # All-cause envelopes
  datIGME1 <- datIGME2[, c('ISO3', 'Year', paste0('dea', ageLow, 'to', ageLow + 4, 'F'),
                           paste0('dea', ageLow, 'to', ageLow + 4, 'M'))]
  names(datIGME1)[3:4] <- c('F', 'M')
  datIGME1 <- as.data.frame(pivot_longer(data = datIGME1, cols = c('F', 'M'), 
                                         names_to = 'Sex', values_to = c('Deaths2')))
  datIGME2 <- datIGME2[, c('ISO3', 'Year', paste0('q', ageLow, 'to', ageLow + 4, 'F'),
                           paste0('q', ageLow, 'to', ageLow + 4, 'M'))]
  names(datIGME2)[3:4] <- c('F', 'M')
  datIGME2 <- as.data.frame(pivot_longer(data = datIGME2, cols = c('F', 'M'), 
                                         names_to = 'Sex', values_to = c('Qx')))
  datIGME2 <- merge(datIGME1, datIGME2, by = idVars)
  rm(datIGME1)
  
} else {
  
  # Crisis-free envelopes
  datIGME <- datIGME[, c('ISO3', 'Year', paste0('dea', ageGroup))]
  names(datIGME)[ncol(datIGME)] <- 'Deaths1'
  datIGME$Sex <- 'B'
  
  # All-cause envelopes
  datIGME2 <- datIGME2[, c('ISO3', 'Year', 
                           paste0('dea', ageGroup),
                           paste0('q', ageGroup))]
  names(datIGME2)[-c(1:2)] <- c('Deaths2', 'Qx')
  datIGME2$Sex <- 'B'
  
}
datIGME <- merge(datIGME, datIGME2, by = idVars)
rm(datIGME2)

