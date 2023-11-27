###################################################################
########################## BEGIN-INPUTS ###########################
###################################################################

# Load packages and session variables if not already loaded
if(!exists("sessionVars")){source("./src/prepare-session.R")
  load("./gen/data-prep/input/session-variables.Rdata")}

# Load input(s)
#if(ageGroup %in% c("00to28","01to04")){dat <-  read.csv("./gen/data-prep/output/db_china_u5.csv")}
if(ageGroup %in% c("05to09","10to14","15to19f", "15to19m")){dat <-  read.csv("./gen/data-prep/output/db_china_5to19.csv")}
if(ageGroup == "05to09"){key_cod <- read.csv("./gen/data-prep/output/key_cod_05to09.csv")}
if(ageGroup == "10to14"){key_cod <- read.csv("./gen/data-prep/output/key_cod_10to14.csv")}
if(ageGroup %in% c("15to19f", "15to19m")){key_cod <- read.csv("./gen/data-prep/output/key_cod_15to19.csv")}

###################################################################
########################## END-INPUTS #############################
###################################################################

### Note: this code will need to be re-written when we incorporate the under-5 data
# Currently only works from predicting 5-19 from DSP data

cod <- unique(key_cod$Reclass)  # Vector with ALL CAUSES OF DEATH (including single-cause estimates)

# Select age group
if (ageLow == 5) {
  dat <- dat[dat$group == "Both 5-9", ]
  dat$Sex <- sexLabels[1]
}
if (ageLow == 10) {
  dat <- dat[dat$group == "Both 10-14", ]
  dat$Sex <- sexLabels[1]
}
if (ageLow == 15) {
  dat <- dat[dat$group %in% c("Female 15-19_(4)", "Male 15-19"), ]
  dat$Sex <- sexLabels[2]
  dat$Sex[dat$group == "Male 15-19"] <- sexLabels[3]
}

# RE-LABEL CAUSES OF DEATH
names(dat)[names(dat) == "csdf3"] <- "dia"
names(dat)[names(dat) == "csdf4"] <- "mea"
names(dat)[names(dat) == "csdf7"] <- "mening"
names(dat)[names(dat) == "csdf9"] <- "lri"
names(dat)[names(dat) == "csdf10"] <- "tb"
names(dat)[names(dat) == "csdf11"] <- "maternal"
names(dat)[names(dat) == "csdf12"] <- "othercd"
names(dat)[names(dat) == "csdf14"] <- "congen"
names(dat)[names(dat) == "csdf15"] <- "neoplasm"
names(dat)[names(dat) == "csdf16"] <- "cardio"
names(dat)[names(dat) == "csdf17"] <- "endo"
names(dat)[names(dat) == "csdf18"] <- "digest"
names(dat)[names(dat) == "csdf19"] <- "otherncd"
names(dat)[names(dat) == "csdf21"] <- "rta"
names(dat)[names(dat) == "csdf22"] <- "drown"
names(dat)[names(dat) == "csdf23"] <- "natdis"
names(dat)[names(dat) == "csdf24"] <- "intvio"
names(dat)[names(dat) == "csdf25"] <- "colvio"
names(dat)[names(dat) == "csdf27"] <- "selfharm"
names(dat)[names(dat) == "csdf28"] <- "otherinj"

# Add MISSING CATEGORIES to match VA COD list
if (!"hiv" %in% names(dat)) dat$hiv <- 0
if (!"mal" %in% names(dat)) dat$mal <- 0
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

# Delete unnecessary columns (HIV will be incorporated later)
dat <- dat[, names(dat) %in% c("year", "Sex", cod)]
dat <- dat[, !names(dat) %in% c("HIV", "Other", "Undetermined")]

# Vector with Causes of death in China (to be used when estimating uncertainty)
#codCHN <- names(dat)[-c(1:2)]

# Country and Year
dat$ISO3 <- "CHN"
names(dat)[names(dat) == "year"] <- "Year"

#----------------------#
# BEGIN NEW 2022.09.28 #

# APPLY ESTIMATES FROM 2019 TO 2020
dat2020 <- dat[dat$Year == 2019, ]
dat2020$Year <- 2020
dat <- rbind(dat, dat2020)
rm(dat2020)

# END NEW              #
#----------------------#

# Hal note: Is this necessary?
# In Pancho"s code the igme envelopes are merged onto china csmfs. check to see if needed.

# # Add IGME envelopes
# dat <- merge(dat, y = datIGME, by = c("ISO3", "Year", "Sex"), all.x = T)
# 
# # Adjust
# dat[, !names(dat) %in% c(idVars, "Deaths1", "Deaths2", "Rate2")] <-
#   dat[, !names(dat) %in% c(idVars, "Deaths1", "Deaths2", "Rate2")] /
#   rowSums(dat[, !names(dat) %in% c(idVars, "Deaths1", "Deaths2", "Rate2")])
# 
# # Tidy up
# dat <- dat[order(dat$ISO3, dat$Year, dat$Sex), ]
# rownames(dat) <- NULL
# 
# # Remove unnecessary objects
# rm(reclass, cod2, orig)

# Tidy up
dat <- dat[order(dat$ISO3, dat$Year, dat$Sex), ]
dat <- dat[,c(idVars, names(dat)[!(names(dat) %in% idVars)])]
rownames(dat) <- NULL

# 15-19 years
if (sexSplit) {
  dat <- subset(dat, Sex == sexLabel)
}

# Remove unnecessary objects
rm(key_cod, cod)

###################################################################
######################### BEGIN-OUTPUTS ###########################
###################################################################

# Save output(s)
write.csv(dat, paste("./gen/prediction/output/csmf_", ageGroup, "CHN.csv", sep=""), row.names = FALSE)


###################################################################
######################### END-OUTPUTS #############################
###################################################################




# FUNCTION FOR THIS

##################################################
####
####   Calculate CSMFs, goodVR
####
##################################################

fn_calc_goodvr <- function(dat, env, key_ctryclass, key_cod){
  
  v_cod <- unique(key_cod$Reclass) # Vector with all CODs (including single-cause estimates)
  
  # Re-label variables
  names(dat)[names(dat) == "iso3"] <- "ISO3"
  names(dat)[names(dat) == "year"] <- "Year"
  names(dat)[names(dat) == "sex"] <- "Sex"
  names(dat)[names(dat) == "neo"] <- "neoplasm"
  env <- env[,c(idVars, "Deaths2","Rate2")]
  names(env)[names(env) == "Deaths2"] <- "Deaths"
  names(env)[names(env) == "Rate2"] <- "Rate"
  
  dat <- dat[dat$age_lb == ageLow, ]
  dat <- dat[dat$Year %in% Years, ]
  dat <- dat[dat$ISO3 %in% key_ctryclass$ISO3[key_ctryclass$Group2010 == "VR"], ]
  dat <- dat[, !names(dat) %in% c("age_lb", "post_source", "post_source2", 
                                  "igmedeaths", "total", "check")]
  
  # Collapse data points when there is no sex-split
  if (sexSplit) {
    dat$Sex[which(dat$Sex == 1)] <- sexLabels[3]
    dat$Sex[which(dat$Sex == 2)] <- sexLabels[2]
  } else {
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
  for (i in 1:length(v_cod)) {
    orig <- key_cod$Original[key_cod$Reclass == v_cod[i]]
    if (length(orig) > 1) {
      dat[, paste(v_cod[i])] <- apply(dat[, paste(orig)], 1, 
                                      function(x) {
                                        if (all(is.na(x))) {
                                          return(NA)
                                        } else return(sum(x, na.rm = T))
                                      })
    } else dat[, paste(v_cod[i])] <- dat[, paste(orig)]
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
  dat <- merge(dat, env, by = idVars, all.x = T)
  rm(env)
  
  # Tidy up
  dat <- dat[order(dat$ISO3, dat$Year, dat$Sex), ]
  rownames(dat) <- NULL
  
  # Save output
  write.csv(dat, paste("./gen/prediction/output/csmf_", ageGroup, "GOODVR.csv", sep=""), row.names = FALSE)
  
}

