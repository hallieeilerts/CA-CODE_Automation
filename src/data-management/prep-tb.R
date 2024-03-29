################################################################################
#' @description Keep age-sex group of interest, impute missing values, set zero TB deaths for country-years with zero crisis-free deaths
#' @return Data frame with c("ISO3", "Year", "Sex", "TB", "tb_lb", "tb_ub", "TBre", "tbre_lb", "tbre_ub")
################################################################################
#' Libraries
#' Inputs
source("./src/prepare-session/set-inputs.R")
source("./src/prepare-session/create-session-variables.R")
dat_tb_u20_WHO    <- read.csv("./data/single-causes/tb/20201023-ProgramTB.csv") # TB Program estimates (WHO) (Updated 23 Oct 2020)
dat_tbAux_u20_WHO <- read.csv("./data/single-causes/tb/20201214-ProgramTB-GMB-MOZ.csv") # Updated estimates for GMB and MOZ
key_ctryclass_u20 <- read.csv("./gen/data-management/output/key_ctryclass_u20.csv")
env               <- read.csv(paste("./gen/data-management/output/env_",ageGroup,".csv", sep = ""))
################################################################################

dat    <- dat_tb_u20_WHO
datAux <- dat_tbAux_u20_WHO

# Combine all tb data
dat <- dat[!dat$iso3 %in% datAux$iso3, ]
dat <- rbind(dat, datAux)
rm(datAux)
names(dat)[names(dat) == "iso3"] <- "ISO3"
names(dat)[names(dat) == "year"] <- "Year"
names(dat)[names(dat) == "sex"] <- "Sex"
names(dat)[names(dat) == "mort.nh.nr.num"] <- "TB"
names(dat)[names(dat) == "mort.nh.nr.lo.num"] <- "tb_lb"
names(dat)[names(dat) == "mort.nh.nr.hi.num"] <- "tb_ub"
names(dat)[names(dat) == "mort.nh.re.num"] <- "TBre"
names(dat)[names(dat) == "mort.nh.re.lo.num"] <- "tbre_lb"
names(dat)[names(dat) == "mort.nh.re.hi.num"] <- "tbre_ub"

# Recode sex variable
dat$Sex[dat$Sex == "MF"] <- sexLabels[1]
dat$Sex[dat$Sex == "F"] <- sexLabels[2]
dat$Sex[dat$Sex == "M"] <- sexLabels[3]

# Extend estimates from latest year ---------------------------------------

# APPLY ESTIMATES FROM 2019 TO 2020 AND 2021
dat2020 <- dat[dat$Year == 2019, ]
dat2020$Year <- 2020
dat <- rbind(dat, dat2020)
dat2020$Year <- 2021
dat <- rbind(dat, dat2020)
dat <- dat[order(dat$ISO3, dat$Year, dat$Sex), ]
rownames(dat) <- NULL
rm(dat2020)


# Keep age and sex group of interest --------------------------------------

# Aggregate sexes for 5-14 years
if(!sexSplit){
  dat <- aggregate(dat[, c("TB", "tb_lb", "tb_ub", "TBre", "tbre_lb", "tbre_ub")], list(dat$ISO3, dat$Year, dat$age_group), sum)
  dat$Sex <- sexLabel
  names(dat)[names(dat) == "Group.1"] <- "ISO3"
  names(dat)[names(dat) == "Group.2"] <- "Year"
  names(dat)[names(dat) == "Group.3"] <- "age_group"
}

# Collapse respiratory and non-respiratory for 15-19 years
if(sexSplit){
  dat$TB <- dat$TB + dat$TBre
  dat$tb_lb <- dat$tb_lb + dat$tbre_lb
  dat$tb_ub <- dat$tb_ub + dat$tbre_ub
  dat <- dat[, c("ISO3", "Year", "Sex", "age_group", "TB", "tb_lb", "tb_ub")]
}

# Keep age/sex group of interest
dat <- dat[which(dat$age_group == paste(ageLow, ageUp, sep = "_") & dat$Sex %in% sexLabel), ]
dat$age_group <- NULL

# Check that all expected countries are included --------------------------

if(sum(!(unique(key_ctryclass_u20$ISO3) %in% dat$ISO3)) > 0){
  warning("Not all countries included in data input.")
  write.table(sort(unique(key_ctryclass_u20$WHOname)[!(unique(key_ctryclass_u20$ISO3) %in% dat$ISO3)]), 
              "./gen/data-management/audit/missing_tb.txt")
}

# Create data frame for countries/years of interest
# For TB data, only HMM and LMM countries (not including China)
df_ctryyears <- data.frame(ISO3 = rep(key_ctryclass_u20$ISO3, each = length(Years)),
                           Year = rep(Years),
                           Sex = sexLabel)

# Merge onto TB data, identifying missing countries/years
dat <- merge(dat, df_ctryyears, by = idVars, all = TRUE)

# Impute missing values ---------------------------------------------------

# Exclude country-years with no deaths in IGME crisis-free envelope
noDeathCountries <- env[which(env$Deaths1 == 0), c("ISO3", "Year")]
noDeathCountries <- merge(dat, noDeathCountries, by = c("ISO3", "Year"))
# Assign zero TB deaths for country-years with zero deaths in crisis-free envelope
noDeathCountries$TB <- 0
hasDeaths <- env[which(env$Deaths1 != 0), c("ISO3", "Year")]
hasDeaths <- merge(dat, hasDeaths, by = c("ISO3", "Year"))

# Impute missing TB values
# For countries with any missing TB values
for(i in unique(hasDeaths$ISO3[is.na(hasDeaths$TB)])){
  # Identify years with no data
  v_years_nodata <- hasDeaths$Year[is.na(hasDeaths$TB) & hasDeaths$ISO3 == i]
  if (length(v_years_nodata) > 0) {
    # Identify years with data
    v_years_data <- hasDeaths$Year[!is.na(hasDeaths$TB) & hasDeaths$ISO3 == i]
    # Fit regressions for point estimate and lower bounds
    if (length(v_years_data) > 0) {
      # Point estimates
      fit <- lm(hasDeaths$TB[!is.na(hasDeaths$TB) & hasDeaths$ISO3 == i] ~ v_years_data)
      hasDeaths$TB[is.na(hasDeaths$TB) & hasDeaths$ISO3 == i] <- coefficients(fit)[1] + v_years_nodata * coefficients(fit)[2]
      # Lower bound
      fit <- lm(hasDeaths$tb_lb[!is.na(hasDeaths$tb_lb) & hasDeaths$ISO3 == i] ~ v_years_data)
      hasDeaths$tb_lb[is.na(hasDeaths$tb_lb) & hasDeaths$ISO3 == i] <- coefficients(fit)[1] + v_years_nodata * coefficients(fit)[2]
      # Upper bound
      fit <- lm(hasDeaths$tb_ub[!is.na(hasDeaths$tb_ub) & hasDeaths$ISO3 == i] ~ v_years_data)
      hasDeaths$tb_ub[is.na(hasDeaths$tb_ub) & hasDeaths$ISO3 == i] <- coefficients(fit)[1] + v_years_nodata * coefficients(fit)[2]  
    }
  }  
}
# Note: I don't think this line is needed. All NA will have been imputed. But be sure to check.
# hasDeaths$TB[which(is.na(hasDeaths$TB))] <- 0

# Respiratory TB
if(respTB) {
  # Impute missing TB values
  # For countries with any missing TB values
  for(i in unique(hasDeaths$ISO3[is.na(hasDeaths$TBre)])){
    # Identify years with no data
    v_years_nodata <- hasDeaths$Year[is.na(hasDeaths$TBre) & hasDeaths$ISO3 == i]
    if (length(v_years_nodata) > 0) {
      # Identify years with data
      v_years_data <- hasDeaths$Year[!is.na(hasDeaths$TBre) & hasDeaths$ISO3 == i]
      # Fit regressions for point estimate and lower bounds
      if (length(v_years_data) > 0) {
        # Point estimates
        fit <- lm(hasDeaths$TBre[!is.na(hasDeaths$TBre) & hasDeaths$ISO3 == i] ~ v_years_data)
        hasDeaths$TBre[is.na(hasDeaths$TBre) & hasDeaths$ISO3 == i] <- coefficients(fit)[1] + v_years_nodata * coefficients(fit)[2]
        # Lower bound
        fit <- lm(hasDeaths$tbre_lb[!is.na(hasDeaths$tbre_lb) & hasDeaths$ISO3 == i] ~ v_years_data)
        hasDeaths$tbre_lb[is.na(hasDeaths$tbre_lb) & hasDeaths$ISO3 == i] <- coefficients(fit)[1] + v_years_nodata * coefficients(fit)[2]
        # Upper bound
        fit <- lm(hasDeaths$tbre_ub[!is.na(hasDeaths$tbre_ub) & hasDeaths$ISO3 == i] ~ v_years_data)
        hasDeaths$tbre_ub[is.na(hasDeaths$tbre_ub) & hasDeaths$ISO3 == i] <- coefficients(fit)[1] + v_years_nodata * coefficients(fit)[2]  
      }
    }  
  }
  # Note: I don't think this line is needed. All NA will have been imputed. But be sure to check.
  # hasDeaths$TBre[which(is.na(hasDeaths$TBre))] <- 0
  # Assign zero TB deaths for country-years with zero deaths in crisis-free envelope
  noDeathCountries$TBre <- 0
}

# Add back in country-years with no deaths
dat <- rbind(hasDeaths, noDeathCountries)

# Tidy up
dat <- dat[, c("ISO3", "Year", "Sex", "TB", "tb_lb", "tb_ub", 
               "TBre", "tbre_lb", "tbre_ub")]
dat <- dat[order(dat$ISO3, dat$Year),]
rownames(dat) <- NULL

# Save output(s) ----------------------------------------------------------

dat_tb <- dat

write.csv(dat, paste("./gen/squeezing/input/dat_tb_", ageGroup, ".csv", sep=""), row.names = FALSE)
