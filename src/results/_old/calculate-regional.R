

###################################################################
########################## BEGIN-INPUTS ###########################
###################################################################

# Load packages and session variables if not already loaded
if(!exists("sessionVars")){source("./src/prepare-session.R")
  load("./gen/data-prep/input/session-variables.Rdata")}

# Load input(s)
dat <- read.csv(paste("./gen/results/output/PointEstimates_National_05to09_", resDate, ".csv", sep =""))


###################################################################
########################## END-INPUTS #############################
###################################################################

# Combine csmfs for modelled countries with VR

# Create unified variable for region
dat$Region <- dat$UNICEFReportRegion1
# If report region 2 is not missing, use it instead
dat$Region[which(dat$UNICEFReportRegion2 != "")] <- dat$UNICEFReportRegion2[which(dat$UNICEFReportRegion2 != "")]

# Causes of death for this age group
v_cod <- codAll[codAll %in% names(dat)]

# Manually add extra regions
df_world <- dat
df_world$Region <- "World"
df_eca <- subset(dat, UNICEFReportRegion1 == "Europe and central Asia")
df_eca$Region <- "Europe and central Asia"
df_ssa <- subset(dat, UNICEFReportRegion1 == "Sub-Saharan Africa")
df_ssa$Region <- "Sub-Saharan Africa"
dat <- rbind(dat, df_world, df_eca, df_ssa)

# Create list of regions, move world to front
v_regions <- sort(unique(dat$Region))
v_regions <- v_regions[c(which(v_regions == "World"), which(v_regions != "World"))]

# Convert CSMFs to deaths
dat[, paste(v_cod)] <- dat[, paste(v_cod)] * dat$Deaths

# Back calculate denominator from deaths and mortality rate
dat$Px <- dat$Deaths/dat$Rate

# Aggregate countries for each region
dat <- ddply(dat, ~Region, function(x){aggregate(x[, c("Deaths", "Px", paste(v_cod))], 
                                                  by = list(x$Year, x$Sex, x$AgeLow, x$AgeUp), sum, na.rm = T)})
names(dat)[1:5] <- c("Region", "Year", "Sex", "AgeLow", "AgeUp")

# Re-calculate CSMFs
dat[, paste(v_cod)] <- dat[, paste(v_cod)] / dat$Deaths
dat[, paste(v_cod)] <- dat[, paste(v_cod)] / rowSums(dat[, paste(v_cod)])

# Re-calculate mortality rate
dat$Rate <- dat$Deaths / dat$Px
# Remove denominator
dat <- dat[, names(dat) != "Px"]

# Order columns
dat <- dat[, c("Region", "Year", "AgeLow", "AgeUp", "Sex", "Deaths", "Rate", paste(codAll[codAll %in% v_cod]))]

# Tidy up
dat$Region <- factor(dat$Region, levels = v_regions, ordered = TRUE)
dat <- dat[order(dat$Region, dat$Year, dat$Sex), ]
dat$Region <- as.character(dat$Region)
rownames(dat) <- NULL



# Run formatting function
#dat <- fn_format_results(dat, key_region, key_ctryclass, codAll)

# Remove unnecessary objects
#rm(csmf_Sqz, csmfVR, key_cod, key_region, key_ctryclass)

###################################################################
######################### BEGIN-OUTPUTS ###########################
###################################################################

# Save output(s)
write.csv(dat, paste("./gen/results/output/PointEstimates_Regional_", ageGroup,"_", format(Sys.Date(), format="%Y%m%d"),".csv", sep=""), row.names = FALSE)

###################################################################
######################### END-OUTPUTS #############################
###################################################################