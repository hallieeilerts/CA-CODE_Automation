
###################################################################
########################## BEGIN-INPUTS ###########################
###################################################################

# Load packages and session variables if not already loaded
if(!exists("sessionVars")){source("./src/prepare-session.R")
  load("./gen/data-prep/input/session-variables.Rdata")}

# Load input(s)
if(ageGroup == "05to09"){dat <- read.csv("./gen/squeezing/temp/dth_AddMeasepi_05to09.csv")
                         datNODEATHS <- read.csv("./gen/squeezing/temp/csmf_05to09NODEATHS.csv")
                         datCHN <- read.csv("./gen/squeezing/temp/dth_SqzCrisisepi_05to09CHN.csv")
                         key_cod <- read.csv("./gen/data-prep/output/key_cod_05to09.csv")}
if(ageGroup == "10to14"){dat <- read.csv("./gen/squeezing/temp/dth_SqzCrisisepi_10to14.csv")
                         datNODEATHS <- read.csv("./gen/squeezing/temp/csmf_10to14NODEATHS.csv")
                         datCHN <- read.csv("./gen/squeezing/temp/dth_SqzCrisisepi_10to14CHN.csv")
                         key_cod <- read.csv("./gen/data-prep/output/key_cod_10to14.csv")}
if(ageGroup == "15to19f"){dat <- read.csv("./gen/squeezing/temp/dth_SqzCrisisepi_15to19f.csv")
                          datNODEATHS <- read.csv("./gen/squeezing/temp/csmf_15to19fNODEATHS.csv")
                          datCHN <- read.csv("./gen/squeezing/temp/dth_SqzCrisisepi_15to19fCHN.csv")
                          key_cod <- read.csv("./gen/data-prep/output/key_cod_15to19.csv")}
if(ageGroup == "15to19m"){dat <- read.csv("./gen/squeezing/temp/dth_SqzCrisisepi_15to19m.csv")
                          datNODEATHS <- read.csv("./gen/squeezing/temp/csmf_15to19mNODEATHS.csv")
                          datCHN <- read.csv("./gen/squeezing/temp/dth_SqzCrisisepi_15to19mCHN.csv")
                          key_cod <- read.csv("./gen/data-prep/output/key_cod_15to19.csv")}

###################################################################
########################## END-INPUTS #############################
###################################################################

cod <- unique(key_cod$Reclass)  # Vector with ALL CAUSES OF DEATH (including single-cause estimates)
cod <- cod[!cod %in% c("Other", "Undetermined")]

# Combine China with HMM/LMM countries
dat <- bind_rows(dat, datCHN)

# Back-transform deaths into fractions
dat[, paste(cod)] <- dat[, paste(cod)]/dat$Deaths2

# Checks
if(any(is.na(rowSums(dat[, paste(cod)])))){
   stop("CSMFs contain NA")
}
if(any(round(rowSums(dat[, paste(cod)]),5) != 1)){
   warning("CSMFs do not add up to 1")
}
# print(round(rowSums(dat[, paste(cod)]),7), digits = 20)
#table(rowSums(dat[, paste(cod)]))
#table(round(rowSums(dat[, paste(cod)]),5))
#dat[which(round(rowSums(dat[, paste(cod)]),5) == 0.99942),]
#dat[which(rowSums(dat[, paste(cod)]) != 1),]
#foo <- dat[which(rowSums(dat[, paste(cod)]) > 1.18),]
#foo[, c("ISO3",paste(cod))]

# Select columns of interest
dat <- dat[, c("ISO3", "Year", "Sex", "Deaths2", "Rate2", paste(cod))]

# Incorporate country-year with 0 crisis-free deaths
if (nrow(datNODEATHS) > 0) {
  datNODEATHS <- datNODEATHS[, names(dat)]
  datNODEATHS[, paste(cod)] <- 0
  dat <- rbind(dat, datNODEATHS)  
}

# Tidy up
names(dat)[names(dat) == "Deaths2"] <- "Deaths"
names(dat)[names(dat) == "Rate2"] <- "Rate"
dat <- dat[order(dat$ISO3, dat$Year, dat$Sex), ]
rownames(dat) <- NULL

# Remove unnecessary objects
rm(cod, datCHN, datNODEATHS)

###################################################################
######################### BEGIN-OUTPUTS ###########################
###################################################################

# Save output(s)
write.csv(dat, paste("./gen/squeezing/output/csmf_Sqz_", ageGroup, ".csv", sep=""), row.names = FALSE)

###################################################################
######################### END-OUTPUTS #############################
###################################################################