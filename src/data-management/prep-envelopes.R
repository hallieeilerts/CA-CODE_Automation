################################################################################
#' @description Format envelopes, merge crisis-free and crisis-included together
#' @return Data frame for age-sex group of interest, c("ISO3", "Year", "Sex", "AgeLow", "AgeUp", "Deaths1", "Rate1", "Deaths2", "Rate2")
#' @return Data frame with all ages, crisis-free only, c("ISO3", "Year", "Sex", "AgeLow", "AgeUp", "Deaths1", "Rate1")
#' @return Data frame with all ages, crisis-included only, c("ISO3", "Year", "Sex", "AgeLow", "AgeUp", "Deaths2", "Rate2")
################################################################################
#' Libraries
require(readxl)
#' Inputs
source("./src/prepare-session/set-inputs.R")
source("./src/prepare-session/create-session-variables.R")
env_crisisfree_u20_igme <- read_excel("./data/igme/envelopes/national/UN IGME 2022 Rates & Deaths_Country Summary (crisis free) 1980-2021 all ages.xlsx")
env_crisisincl_u20_igme <- read_excel("./data/igme/envelopes/national/UN IGME 2022 Rates & Deaths_Country Summary 1980-2021 all ages.xlsx")
key_ctryclass           <- read.csv("./gen/data-management/output/key_ctryclass_u20.csv")
################################################################################

# Function to tidy up UN IGME envelopes
fn_tidy_up_envelopes <- function(dat, var1, var2, years) {
  
  ## dat      Envelopes 
  ## var1     Name of the first output variable
  ## var2     Name of the second output variable
  ## years    Years of interest

  # Identify rates data
  colKeep2 <- which(substr(names(dat), 1, 3) == "NMR" |
                      substr(names(dat), 1, 16) == "Months1to59.Rate" |
                      substr(names(dat), 1, 6) == "MR5to9" | 
                      substr(names(dat), 1, 7) == "MR5to14" |
                      substr(names(dat), 1, 8) == "MR10to14" |
                      substr(names(dat), 1, 8) == "MR15to19")
  dat2 <- dat[, c(1, 2, colKeep2)]
  dat2 <- dat2[, -1]
  names(dat2)[1] <- "ISO3"
  
  # Identify deaths data
  colKeep1 <- which(substr(names(dat), 1, 15) == "Neonatal.Deaths" | 
                      substr(names(dat), 1, 18) == "Months1to59.Deaths" | 
                      substr(names(dat), 1, 11) == "Deaths.5to9" |
                      substr(names(dat), 1, 12) == "Deaths.5to14" |
                      substr(names(dat), 1, 13) == "Deaths.10to14" |
                      substr(names(dat), 1, 13) == "Deaths.15to19")
  dat <- dat[, c(1, 2, colKeep1)]
  dat <- dat[, -1]
  names(dat)[1] <- "ISO3"
  
  ## Deaths
  
  # Identify sex-specific columns and save in separate data frame
  idfem <- which(grepl(pattern = ".f", names(dat)))
  idmen <- which(grepl(pattern = ".m", names(dat)))
  dat1 <- dat[, c(1, idfem, idmen)]
  
  # Remove sex-specific columns from main data frame
  dat <- dat[, -c(idfem, idmen)]
  
  # Re-shape sex-combined data frame
  dat <- cbind(dat[1], stack(dat[2:ncol(dat)]))
  dat$Age1 <- sapply(dat$ind, function(x){ strsplit(as.character(x), ".", fixed = TRUE)[[1]][1] })
  dat$Age2 <- sapply(dat$ind, function(x){ strsplit(as.character(x), ".", fixed = TRUE)[[1]][2] })
  dat$Year <- sapply(dat$ind, function(x){ strsplit(as.character(x), ".", fixed = TRUE)[[1]][3] })
  names(dat)[names(dat) == "values"] <- var1
  dat$ind <- NULL
  
  # Account for differences in age column formatting for under-5s and 5-19
  dat[dat == "Deaths"] <- NA
  dat$Age <- dat$Age1
  dat$Age[is.na(dat$Age)] <- dat$Age2[is.na(dat$Age)]
  dat <- dat[ , -which(names(dat) %in% c("Age1", "Age2"))]
  dat$Sex <- sexLabels[1]
  
  # Re-shape sex-specific data frame
  dat1 <- cbind(dat1[1], stack(dat1[2:ncol(dat1)]))
  dat1$Age <- sapply(dat1$ind, function(x){ strsplit(as.character(x), ".", fixed = TRUE)[[1]][2] })
  dat1$Year <- sapply(dat1$ind, function(x){ strsplit(as.character(x), ".", fixed = TRUE)[[1]][3] })
  dat1$Sex <- sapply(dat1$ind, function(x){ strsplit(as.character(x), ".", fixed = TRUE)[[1]][4] })
  names(dat1)[names(dat1) == "values"] <- var1
  dat1$ind <- NULL
  
  # Combine sex-combined and sex-specific data frames
  dat <- rbind(dat, dat1)
  
  # Create 5 to 19 age group
  dat1 <- merge(dat[which(dat$Age == "5to14"), ], 
                dat[which(dat$Age == "15to19" & dat$Sex == sexLabels[1]), c("ISO3", "Year", var1)], by = c("ISO3", "Year"))
  dat1[, paste0(var1, ".y")] <- dat1[, paste0(var1, ".x")] + dat1[, paste0(var1, ".y")]
  dat1$Age <- "5to19"
  dat1 <- dat1[, !names(dat1) == paste0(var1, ".x")]
  names(dat1)[names(dat1) == paste0(var1, ".y")] <- var1
  
  # Combine data frame for 5-19 with other ages
  dat <- rbind(dat1, dat)
  rm(dat1)
  
  ## Rates
  
  # Identify sex-specific columns and save in separate data frame
  idfem <- which(grepl(pattern = ".f", names(dat2)))
  idmen <- which(grepl(pattern = ".m", names(dat2)))
  dat1 <- dat2[, c(1, idfem, idmen)]
  
  # Remove sex-specific columns from main data frame
  dat2 <- dat2[, -c(idfem, idmen)]
  
  # Re-shape sex-combined data frame
  dat2 <- cbind(dat2[1], stack(dat2[2:ncol(dat2)]))
  dat2$Age <- sub("\\..*", "", dat2$ind)   # Extract text after last period
  dat2$Year <- sub('.*\\.', '', dat2$ind)  # Extract text before first period
  names(dat2)[names(dat2) == "values"] <- var2
  dat2$ind <- NULL
  
  # Account for differences in age column formatting for under-5s and 5-19
  dat2$Sex <- sexLabels[1]
  
  # Re-shape sex-specific data frame
  dat1 <- cbind(dat1[1], stack(dat1[2:ncol(dat1)]))
  dat1$Age <- sapply(dat1$ind, function(x){ strsplit(as.character(x), ".", fixed = TRUE)[[1]][1] })
  dat1$Year <- sapply(dat1$ind, function(x){ strsplit(as.character(x), ".", fixed = TRUE)[[1]][2] })
  dat1$Sex <- sapply(dat1$ind, function(x){ strsplit(as.character(x), ".", fixed = TRUE)[[1]][3] })
  names(dat1)[names(dat1) == "values"] <- var2
  dat1$ind <- NULL
  
  # Combine data frames
  dat2 <- rbind(dat2, dat1)
  rm(dat1)
  dat2$Age[dat2$Age == "NMR"] <- "Neonatal"
  dat2$Age <- sub("MR", "", dat2$Age)
  
  # 5 to 19 age group
  dat1 <- merge(dat2[which(dat2$Age == "5to14"), c(idVars[1:2], var2)], 
                dat2[which(dat2$Age == "15to19" & dat2$Sex == sexLabels[1]), c(idVars[1:2], var2)], by = idVars[1:2])
  dat1[, paste0(var2, ".y")] <- (1 - (1 - dat1[, paste0(var2, ".x")] / 1000) *
                                   (1 - dat1[, paste0(var2, ".y")] / 1000)) * 1000
  dat1$Age <- "5to19"
  dat1 <- dat1[, !names(dat1) == paste0(var2, ".x")]
  names(dat1)[names(dat1) == paste0(var2, ".y")] <- var2
  dat1$Sex <- sexLabels[1]
  dat2 <- rbind(dat1, dat2)
  rm(dat1)
  
  # Merge rates onto deaths
  dat <- merge(dat, dat2, by = c(idVars, "Age"))
  rm(dat2)
  
  ## Tidy up
  
  # Sex labels
  dat$Sex[dat$Sex == "f"] <- sexLabels[2]
  dat$Sex[dat$Sex == "m"] <- sexLabels[3]
  
  # Age bounds
  dat$AgeLow <- sub("to.*", "", dat$Age)
  dat$AgeUp <- sub(".*to", "", dat$Age)
  dat$AgeLow <- ifelse(dat$Age == "Neonatal", 0, dat$AgeLow)
  dat$AgeUp <- ifelse(dat$Age == "Neonatal", 28/365.25, dat$AgeUp)
  dat$AgeLow <- ifelse(dat$Age == "Months1to59", 1/12, dat$AgeLow)
  dat$AgeUp <- ifelse(dat$Age == "Months1to59", 1, dat$AgeUp)
  dat$Age <- NULL
  
  # Years of interest
  if (!is.null(years)) dat <- dat[dat$Year %in% years, ]
  
  # Tidy up
  dat <- dat[, c("ISO3", "Year", "AgeLow", "AgeUp", "Sex", var1, var2)] 
  dat$Year <- as.numeric(dat$Year)
  dat$AgeLow <- as.numeric(dat$AgeLow)
  dat$AgeUp <- as.numeric(dat$AgeUp)
  dat <- dat[order(dat$ISO3, dat$AgeLow, dat$AgeUp, dat$Sex, dat$Year), ]
  rownames(dat) <- NULL

  # Output
  return(dat)

}

# Tidy up envelopes
dat1 <- fn_tidy_up_envelopes(env_crisisfree_u20_igme, var1 = "Deaths1", var2 = "Rate1", years = Years)
dat2 <- fn_tidy_up_envelopes(env_crisisincl_u20_igme, var1 = "Deaths2", var2 = "Rate2", years = Years)

# Merge crisis-free and crisis-included
dat <- merge(dat1, dat2, by = c("ISO3","Year","AgeLow", "AgeUp", "Sex"))

# Keep age/sex group of interest
dat <- dat[which(dat$AgeLow == ageLow & dat$AgeUp == ageUp & dat$Sex %in% sexLabel), ]

# Select countries of interest
dat <- dat[which(dat$ISO3 %in% unique(key_ctryclass$ISO3)), ]

# Quality checks ----------------------------------------------------------

# Check that crisis-free envelopes are not larger than crisis-included
df_check <- dat
df_check$ind1 <- ifelse(df_check$Deaths1 > df_check$Deaths2, 1, 0)
if(sum(df_check$ind1) > 0){
  stop("Crisis-free envelopes larger than crisis-included.")
}

# Save output(s) ----------------------------------------------------------

# These envelopes used for prediction database
write.csv(dat1, paste("./gen/data-management/output/env_crisisfree_u20.csv", sep = ""), row.names = FALSE)
write.csv(dat2, paste("./gen/data-management/output/env_crisisincl_u20.csv", sep = ""), row.names = FALSE)
# This one is sex-specific and used in all other cases
write.csv(dat, paste("./gen/data-management/output/env_",ageGroup,".csv", sep = ""), row.names = FALSE)

# Remove unnecessary objects
rm(env_crisisfree_u20_igme, env_crisisincl_u20_igme, key_ctryclass, 
   dat1, dat2, fn_tidy_up_envelopes, df_check)

