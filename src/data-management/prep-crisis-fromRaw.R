################################################################################
#' Libraries
require(data.table)
#' Inputs
dat_crisis_u20_WPP    <- read.csv("./data/single-causes/crisis/asmr_crisis_wpp.csv")
key_ctryclass_u20     <- read.csv("./gen/data-management/output/key_ctryclass_u20.csv")
################################################################################

dat <- dat_crisis_u20_WPP
key <- key_ctryclass_u20 

# Recode Cote d'Ivoire in country name key
key$WHOname[key$ISO3 == "CIV"] <- "Côte d'Ivoire"

# Merge on country code ---------------------------------------------------

df_data <- merge(dat, key[,c("ISO3", "WHOname")], by.x = "LocName", by.y = "WHOname")

# See which countries are missing ISO3 in the data
v_missing <- unique(dat$LocName)[!(unique(dat$LocName) %in% unique(df_data$LocName))]
v_missing <- sort(v_missing)
length(v_missing) # 41
v_missing
# Drop non-countries/countries we don't model
v_missing <- v_missing[!(v_missing %in% c("American Samoa", "Anguilla", "Aruba", "Bermuda", "British Virgin Islands", "Caribbean Netherlands", "Cayman Islands", "China, Hong Kong SAR" ,"China, Macao SAR", "China, Taiwan Province of China", "Curaçao", "Faeroe Islands", "French Guiana", "French Polynesia", "Gibraltar", "Guadeloupe", "Guam", "Isle of Man", "Kosovo (United Nations administered region under security council resolution 1244)", "Liechtenstein", "Martinique", "Mayotte", "Montserrat", "New Caledonia", "Northern Mariana Islands", "Puerto Rico", "Réunion", "Saint-Barthélemy", "Saint-Martin (French part)", "Sint Maarten (Dutch part)", "Turks and Caicos Islands", "United States Virgin Islands", "Wallis and Futuna Islands", "Western Sahara"))]
length(v_missing) # 7
v_missing

# Manual matching
df_manual <- subset(dat, LocName %in% v_missing)
df_manual$LocName[df_manual$LocName == v_missing[1]] <- "Democratic People's Republic of Korea"
df_manual$LocName[df_manual$LocName == v_missing[2]] <- "Democratic Republic of the Congo" 
df_manual$LocName[df_manual$LocName == v_missing[3]] <- "Lao People's Democratic Republic"
df_manual$LocName[df_manual$LocName == v_missing[4]] <- "Micronesia (Federated States of)"
df_manual$LocName[df_manual$LocName == v_missing[5]] <- "Republic of North Macedonia"
df_manual$LocName[df_manual$LocName == v_missing[6]] <- "West Bank and Gaza Strip"
df_manual$LocName[df_manual$LocName == v_missing[7]] <- "United Kingdom of Great Britain and Northern Ireland"
df_manual <- merge(df_manual, key[,c("ISO3", "WHOname")], by.x = "LocName", by.y = "WHOname")

# Combine all countries
df_data <- rbind(df_data, df_manual)

# Reshape -----------------------------------------------------------------

# Create lower bound for age
df_data$age_lb <- df_data$age_label
df_data$age_lb[df_data$age_lb == "infant"] <- 0
df_data$age_lb[df_data$age_lb == "x1to4"] <- 1
df_data$age_lb[df_data$age_lb == "x5"] <- 5
df_data$age_lb[df_data$age_lb == "x10"] <- 10
df_data$age_lb[df_data$age_lb == "x15"] <- 15

# Subset ages under-20
df_data <- subset(df_data, age_lb %in% c(0, 1, 5, 10, 15))

# Subset years >= 2000
df_data <- subset(df_data, Year >= 2000)

# Reshape columns for deaths/mortality rates/sex/uncertainty and recombine
v_keepcols <- c("ISO3", "Year", "age_lb", "EventType")
df_dths <- df_data[,c(v_keepcols, "dth_both", "dth_female", "dth_male")]
df_dths <- melt(setDT(df_dths), id.vars = c("ISO3", "Year", "age_lb", "EventType"), value.name = "dth")
df_dths$variable <- sub('.*\\_', '', df_dths$variable)
df_dths$Sex <- sexLabels[1]
df_dths$Sex[df_dths$variable == "female"] <- sexLabels[2]
df_dths$Sex[df_dths$variable == "male"] <- sexLabels[3]
df_dths <- data.frame(df_dths)[ , -which(names(df_dths) %in% c("variable"))]
df_mx <- df_data[,c(v_keepcols, "mr_both", "mr_both_lo", "mr_both_hi")]
df_mx_f <- df_data[,c(v_keepcols, "mr_female", "mr_female_lo", "mr_female_hi")]
df_mx_m <- df_data[,c(v_keepcols, "mr_male", "mr_male_lo", "mr_male_hi")]
names(df_mx) <- c(v_keepcols, "mx", "mx_lb", "mx_ub")
names(df_mx_f) <- c(v_keepcols, "mx", "mx_lb", "mx_ub")
names(df_mx_m) <- c(v_keepcols, "mx", "mx_lb", "mx_ub")
df_mx$Sex <- sexLabels[1]
df_mx_f$Sex <- sexLabels[2]
df_mx_m$Sex <- sexLabels[3]
df_mx <- rbind(df_mx, df_mx_f, df_mx_m)
df_data <- merge(df_dths, df_mx, by = c(v_keepcols, "Sex"))

# Continue on with Diana's STATA code...

