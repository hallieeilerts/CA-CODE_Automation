
###################################################################
########################## BEGIN-INPUTS ###########################
###################################################################

# Load packages and session variables if not already loaded
if(!exists("sessionVars")){source("./src/prepare-session.R")
  load("./gen/data-prep/input/session-variables.Rdata")}

# Load input(s)
dth_malaria_5to19 <-  read.csv("./gen/prediction/input/dth_malaria_5to19.csv")
csmf_malaria_01to04HMM <-  read.csv("./gen/prediction/input/csmf_malaria_01to04HMM.csv")
if(ageGroup == "05to09"){dat <- read.csv("./gen/prediction/output/csmf_05to09HMM.csv")}
if(ageGroup == "10to14"){dat <- read.csv("./gen/prediction/output/csmf_10to14HMM.csv")}

###################################################################
########################## END-INPUTS #############################
###################################################################

# Vector with ALL CAUSES OF DEATH (including single-cause estimates)
#cod <- unique(key_cod$Reclass)
#cod[cod == "OtherCD"] <- "OtherCMPN"
#cod <- cod[!cod %in% c("Other", "Undetermined")]

v_cod <- names(dat)
v_cod <- v_cod[!(v_cod %in% idVars)]

# Merge on malaria deaths
dat <- merge(dat, dth_malaria_5to19, by = c("ISO3", "Year"), all.x = T)

# Identify cases with 0 malaria
idMal <- which(dat$dth_malaria_5to19 == 0 | is.na(dat$dth_malaria_5to19))

# Force malaria to be 0
if (length(idMal) > 0) {
  dat[idMal, paste(v_cod)] <- dat[idMal, paste(v_cod)]/rowSums(dat[idMal, paste(v_cod[v_cod != "Malaria"])])
  dat[idMal, "Malaria"] <- 0
}
  
# Remove unnecessary data
v_remove <- names(dth_malaria_5to19)[!(names(dth_malaria_5to19) %in% idVars)]
dat <- dat[, !(names(dat) %in% v_remove)]
  
# Cap malaria to 1-59-month fractions
dat <- merge(dat, csmf_malaria_01to04HMM, by = c("ISO3", "Year"), all.x = T)
  
# Assign 0 to NA (if any)
dat$csmf_malaria_01to04[which(is.na(dat$csmf_malaria_01to04))] <- 0

# Identify cases to update malaria
idMal <- which(dat$Malaria > dat$csmf_malaria_01to04)
  
# Cap malaria
if (length(idMal) > 0) {
  dat$Malaria[idMal] <- dat$csmf_malaria_01to04[idMal]
  idCod <- v_cod[v_cod != "Malaria"]
  dat[idMal, paste(idCod)] <- dat[idMal, paste(idCod)] / rowSums(dat[idMal, paste(paste(idCod))])
  dat[idMal, paste(idCod)] <- dat[idMal, paste(idCod)] * (1 - dat$Malaria[idMal])
  rm(idCod)
}
  
# Remove unnecessary data
v_remove <- names(csmf_malaria_01to04HMM)[!(names(csmf_malaria_01to04HMM) %in% idVars)]
dat <- dat[, !(names(dat) %in% v_remove)]
  
# Remove unnecessary objects
rm(dth_malaria_5to19, csmf_malaria_01to04HMM, v_remove, v_cod)

###################################################################
######################### BEGIN-OUTPUTS ###########################
###################################################################

# Save output(s)
write.csv(dat, paste("./gen/prediction/output/csmf_CapMalaria_", ageGroup, "HMM.csv", sep=""), row.names = FALSE)

###################################################################
######################### END-OUTPUTS #############################
###################################################################


