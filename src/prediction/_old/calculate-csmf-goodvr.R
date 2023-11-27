###################################################################
########################## BEGIN-INPUTS ###########################
###################################################################

# Load packages and session variables if not already loaded
if(!exists("sessionVars")){source("./src/prepare-session.R")
  load("./gen/data-prep/input/session-variables.Rdata")}

# Load input(s)
dat <- read.csv("./gen/data-prep/output/db_vr_5to19GOODVR.csv")
key_ctryclass <- read.csv("./gen/data-prep/output/key_ctryclass_u20.csv")
if(ageGroup == "05to09"){key_cod <- read.csv("./gen/data-prep/output/key_cod_05to09.csv")
                         datIGME <- read.csv("./gen/data-prep/output/env_05to09.csv")}
if(ageGroup == "10to14"){key_cod <- read.csv("./gen/data-prep/output/key_cod_10to14.csv")
                         datIGME <- read.csv("./gen/data-prep/output/env_10to14.csv")}
if(ageGroup == "15to19f"){key_cod <- read.csv("./gen/data-prep/output/key_cod_15to19.csv")
                          datIGME <- read.csv("./gen/data-prep/output/env_15to19f.csv")}
if(ageGroup == "15to19m"){key_cod <- read.csv("./gen/data-prep/output/key_cod_15to19.csv")
                          datIGME <- read.csv("./gen/data-prep/output/env_15to19m.csv")}

###################################################################
########################## END-INPUTS #############################
###################################################################

countryClass <- key_ctryclass
cod <- unique(key_cod$Reclass) # Vector with ALL CAUSES OF DEATH (including single-cause estimates)

# Re-label variables
names(dat)[names(dat) == "iso3"] <- "ISO3"
names(dat)[names(dat) == "year"] <- "Year"
names(dat)[names(dat) == "sex"] <- "Sex"
names(dat)[names(dat) == "neo"] <- "neoplasm"
datIGME <- datIGME[,c(idVars, "Deaths2","Rate2")]
names(datIGME)[names(datIGME) == "Deaths2"] <- "Deaths"
names(datIGME)[names(datIGME) == "Rate2"] <- "Rate"

dat <- dat[dat$AgeLow == ageLow, ]
dat <- dat[dat$Year %in% Years, ]
dat <- dat[dat$ISO3 %in% countryClass$ISO3[countryClass$Group2010 == "VR"], ]
dat <- dat[, !names(dat) %in% c("AgeLow", "post_source", "post_source2", 
                                         "igmedeaths", "total", "check")]

# Collapse data points when there is no sex-split
if (sexSplit) {
  dat$Sex[which(dat$Sex == 1)] <- sexLabels[3]
  dat$Sex[which(dat$Sex == 2)] <- sexLabels[2]
} else {
  dat$Sex <- NULL
  dat <- aggregate(dat[, -c(1, 2)], list(dat$ISO3, dat$Year), sum)
  dat$Sex <- sexLabels[1]
} 
# If data points were aggregated, need to rename columns
names(dat)[names(dat) == "Group.1"] <- "ISO3"
names(dat)[names(dat) == "Group.2"] <- "Year"
#names(dat)[1:2] <- idVars[1:2]

# Add "typhoid", "other" and "undt" to match VA COD list
if (!"typhoid" %in% names(dat)) dat$typhoid <- 0
if (!"other" %in% names(dat)) dat$other <- 0
if (!"undt" %in% names(dat)) dat$undt <- 0

# Re-classify causes of death
for (i in 1:length(cod)) {
  orig <- key_cod$Original[key_cod$Reclass == cod[i]]
  if (length(orig) > 1) {
    dat[, paste(cod[i])] <- apply(dat[, paste(orig)], 1, 
                                      function(x) {
                                        if (all(is.na(x))) {
                                          return(NA)
                                        } else return(sum(x, na.rm = T))
                                      })
  } else dat[, paste(cod[i])] <- dat[, paste(orig)]
}

# Delete unnecessary columns
idExclude <- which(!key_cod$Original %in% key_cod$Reclass)
if (length(idExclude) > 0) {
  dat <- dat[, !names(dat) %in% paste(key_cod$Original[idExclude])]
}
dat <- dat[, !names(dat) %in% c("Other", "Undetermined")]

# Calculate fractions
dat[, !names(dat) %in% idVars] <-
  dat[, !names(dat) %in% idVars] / rowSums(dat[, !names(dat) %in% idVars])

# Adjust when 0 deaths
idAdjust <- which(is.na(dat$OtherCMPN))
if (length(idAdjust) > 0) {
  for (i in idAdjust) {
    if (dat$Year[i] == min(Years)) {
      dat[i, !names(dat) %in% idVars] <-
        dat[i+1, !names(dat) %in% idVars]  
    } else {
      dat[i, !names(dat) %in% idVars] <-
        dat[i-1, !names(dat) %in% idVars]
    }
  }
}
rm(i, idAdjust, idExclude, orig)

#----------------------#
# BEGIN NEW 2022.09.28 #

# APPLY ESTIMATES FROM 2019 TO 2020 AND 2021
dat2020 <- dat[dat$Year == 2019, ]
dat2020$Year <- 2020
dat <- rbind(dat, dat2020)
dat2020$Year <- 2021
dat <- rbind(dat, dat2020)
rm(dat2020)

# END NEW              #
#----------------------#

# 15-19 years
if (sexSplit) {
  dat <- subset(dat, Sex == sexLabel)
}

# Merge on IGME envelopes
dat <- merge(dat, datIGME, by = idVars, all.x = T)
rm(datIGME)

# Tidy up
dat <- dat[order(dat$ISO3, dat$Year, dat$Sex), ]
rownames(dat) <- NULL

# Remove unnecessary objects
rm(key_cod, cod, key_ctryclass_u20, countryClass)

# old <- read.csv("./gen/prediction/output/csmf_05to09GOODVR.csv")
# old <- melt(setDT(old), id.vars = idVars)
# new <- dat
# new <- melt(setDT(new), id.vars = idVars)
# test <- merge(old, new, by =c("ISO3","Year", "Sex", "variable"), suffixes = c("old","new"))
# test$dif <- test$valueold - test$valuenew
# max(test$dif)

###################################################################
######################### BEGIN-OUTPUTS ###########################
###################################################################

# Save output(s)
write.csv(dat, paste("./gen/prediction/output/csmf_", ageGroup, "GOODVR.csv", sep=""), row.names = FALSE)

###################################################################
######################### END-OUTPUTS #############################
###################################################################
