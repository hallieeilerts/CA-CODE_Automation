##############################################################
# Format regional uncertainty files for Data Portal upload
##############################################################

#' Notes:
#' I offered to calculate regional uncertainty files on 2023-09-28 since I had already gotten Pancho's code working for this.
#' My results need to be formatted to match his and Jamie's.
#' There are very few differences, just a few related to capitalization of strings or naming of regions.
#' The region names are different because they have to match what UN IGME uses on the Data Portal.

# Clear environment
rm(list = ls())

# Example files to match from Jamie and Pancho
ui_1to59REG <- read.csv("C:/Users/HEilerts/Dropbox/CA-CODE/CA-CODE_2021_SimpleUpdate/DataPortal/Uncertainty1to59m-Regional.csv")
ui_5to9 <- read.csv("./data/previous-results/2000-2021/Uncertainty5to9-National.csv")
ui_10to14 <- read.csv("./data/previous-results/2000-2021/Uncertainty10to14-National.csv")
ui_15to19 <- read.csv("./data/previous-results/2000-2021/Uncertainty15to19-National.csv")
csmf_5to9REG <- read.csv("./data/previous-results/2000-2021/PointEstimates5to9-Regional.csv")

# Load in my final results
final_resDate <- "20231002"
dat_05to09 <- read.csv(paste("./gen/results/output/Uncertainty_Regional_05to09_", final_resDate, ".csv", sep = "")) 
dat_10to14 <- read.csv(paste("./gen/results/output/Uncertainty_Regional_10to14_", final_resDate, ".csv", sep = "")) 
dat_15to19f <- read.csv(paste("./gen/results/output/Uncertainty_Regional_15to19f_", final_resDate, ".csv", sep = "")) 
dat_15to19m <- read.csv(paste("./gen/results/output/Uncertainty_Regional_15to19m_", final_resDate, ".csv", sep = "")) 

# Combine sexes
dat_15to19 <- rbind(dat_15to19f, dat_15to19m)

# Recode sexes from "Both" to "Total"
dat_05to09$Sex <- "Total"
dat_10to14$Sex <- "Total"

# Recode age
v_othercol <- names(dat_05to09)[!(names(dat_05to09) %in% c("Region","Year", "AgeLow", "AgeUp"))]
dat_05to09$Age <- "5 to 9 years"
dat_05to09 <- dat_05to09[,c("Region", "Year", "Age", v_othercol)]
v_othercol <- names(dat_10to14)[!(names(dat_10to14) %in% c("Region","Year", "AgeLow", "AgeUp"))]
dat_10to14$Age <- "10 to 14 years"
dat_10to14 <- dat_10to14[,c("Region", "Year", "Age", v_othercol)]
v_othercol <- names(dat_15to19)[!(names(dat_15to19) %in% c("Region","Year", "AgeLow", "AgeUp"))]
dat_15to19$Age <- "15 to 19 years"
dat_15to19 <- dat_15to19[,c("Region", "Year", "Age", v_othercol)]

# Recode Quantile values
dat_05to09$Quantile <- tolower(dat_05to09$Quantile)
dat_10to14$Quantile <- tolower(dat_10to14$Quantile)
dat_15to19$Quantile <- tolower(dat_15to19$Quantile)
dat_05to09$Quantile[dat_05to09$Quantile ==  "point"] <- "value"
dat_10to14$Quantile[dat_10to14$Quantile ==  "point"] <- "value"
dat_15to19$Quantile[dat_15to19$Quantile ==  "point"] <- "value"
dat_05to09$Quantile <- factor(dat_05to09$Quantile, levels = c("lower", "value", "upper"), ordered = TRUE)
dat_10to14$Quantile <- factor(dat_10to14$Quantile, levels = c("lower", "value", "upper"), ordered = TRUE)
dat_15to19$Quantile <- factor(dat_15to19$Quantile, levels = c("lower", "value", "upper"), ordered = TRUE)

# Order variable columns
dat_05to09$Variable <- factor(dat_05to09$Variable, levels = c("Deaths", "Fraction", "Rate"), ordered = TRUE)
dat_10to14$Variable <- factor(dat_10to14$Variable, levels = c("Deaths", "Fraction", "Rate"), ordered = TRUE)
dat_15to19$Variable <- factor(dat_15to19$Variable, levels = c("Deaths", "Fraction", "Rate"), ordered = TRUE)

# Match regional names in example files
sort(unique(ui_1to59REG$Region))
sort(unique(csmf_5to9REG$Region))
sort(unique(dat_05to09$Region))
unique(dat_05to09$Region)[!(unique(dat_05to09$Region) %in% unique(csmf_5to9REG$Region))]
unique(csmf_5to9REG$Region)[!(unique(csmf_5to9REG$Region) %in% unique(dat_05to09$Region))]
unique(ui_1to59REG$Region)[!(unique(ui_1to59REG$Region) %in% unique(dat_05to09$Region))]
# Drop "Europe and central Asia" "Sub-Saharan Africa" from my file
dat_05to09 <- subset(dat_05to09, !(Region %in% c("Europe and central Asia", "Sub-Saharan Africa")))
dat_10to14 <- subset(dat_10to14, !(Region %in% c("Europe and central Asia", "Sub-Saharan Africa")))
dat_15to19 <- subset(dat_15to19, !(Region %in% c("Europe and central Asia", "Sub-Saharan Africa")))
length(unique(dat_05to09$Region)) # 10
length(unique(dat_10to14$Region)) # 10
length(unique(dat_15to19$Region)) # 10
# Convert to ordered factor
v_regorder <- unique(csmf_5to9REG$Region)
dat_05to09$Region <- factor(dat_05to09$Region, levels = v_regorder, ordered = TRUE)
dat_10to14$Region <- factor(dat_10to14$Region, levels = v_regorder, ordered = TRUE)
dat_15to19$Region <- factor(dat_15to19$Region, levels = v_regorder, ordered = TRUE)
unique(dat_05to09$Region)
unique(dat_10to14$Region)
unique(dat_15to19$Region)

# Sort columns if necessary
names(csmf_5to9REG)
names(ui_5to9)
names(ui_10to14)
names(ui_15to19)
names(dat_05to09)
names(dat_10to14)
names(dat_15to19)

# Check number of rows
nrow(csmf_5to9REG) # 660
nrow(dat_05to09) # 1980 = 660*3
nrow(dat_10to14) # 1980
nrow(dat_15to19) # 3960 = 1980*2

# Sort rows
dat_05to09 <- dat_05to09[order(dat_05to09$Region, dat_05to09$Year, dat_05to09$Variable, dat_05to09$Quantile),]
dat_10to14 <- dat_10to14[order(dat_10to14$Region, dat_10to14$Year, dat_10to14$Variable, dat_10to14$Quantile),]
dat_15to19 <- dat_15to19[order(dat_15to19$Region, dat_15to19$Year, dat_15to19$Sex, dat_15to19$Variable, dat_15to19$Quantile),]

# Save and export
write.csv(dat_05to09, "./gen/adhoc-requests/output/Uncertainty5to9-Regional.csv", row.names = FALSE)
write.csv(dat_10to14, "./gen/adhoc-requests/output/Uncertainty10to14-Regional.csv", row.names = FALSE)
write.csv(dat_15to19, "./gen/adhoc-requests/output/Uncertainty15to19-Regional.csv", row.names = FALSE)

##############################################################
# Format national uncertainty files for Data Portal upload
##############################################################

#' Notes:
#' Pancho asked me to also calculate the national uncertainty files on 2023-10-17 since he has not had time to re-do them with Jamie's most up-to-date results file (child_cod_2000-2021.dta).
#' The one he used was missing Cote d'Ivoire, so malaria was capped at zero for 5-9 and 10-14.
#' My results need to be formatted to match his and Jamie's.
#' There are very few differences, just a few related to capitalization of strings or naming of regions.
#' The region names are different because they have to match what UN IGME uses on the Data Portal.

# Clear environment
rm(list = ls())

# Example files to match from Pancho
ui_5to9 <- read.csv("./data/previous-results/2000-2021/Uncertainty5to9-National.csv")
ui_10to14 <- read.csv("./data/previous-results/2000-2021/Uncertainty10to14-National.csv")
ui_15to19 <- read.csv("./data/previous-results/2000-2021/Uncertainty15to19-National.csv")
head(ui_5to9)
head(ui_10to14)
head(ui_15to19)

# Load in my final results
final_resDate <- "20231002"
dat_05to09 <- read.csv(paste("./gen/results/output/Uncertainty_National_05to09_", final_resDate, ".csv", sep = "")) 
dat_10to14 <- read.csv(paste("./gen/results/output/Uncertainty_National_10to14_", final_resDate, ".csv", sep = "")) 
dat_15to19f <- read.csv(paste("./gen/results/output/Uncertainty_National_15to19f_", final_resDate, ".csv", sep = "")) 
dat_15to19m <- read.csv(paste("./gen/results/output/Uncertainty_National_15to19m_", final_resDate, ".csv", sep = "")) 
head(dat_05to09)
head(dat_10to14)
head(dat_15to19f)
head(dat_15to19m)

# Combine sexes
dat_15to19 <- rbind(dat_15to19f, dat_15to19m)

# Recode sexes from "Both" to "Total"
dat_05to09$Sex <- "Total"
dat_10to14$Sex <- "Total"

# Recode age
v_othercol <- names(dat_05to09)[!(names(dat_05to09) %in% c("ISO3","Year", "AgeLow", "AgeUp", "Model", "FragileState", "WHOname", "SDGregion","UNICEFReportRegion1", "UNICEFReportRegion2"))]
dat_05to09$Age <- "5 to 9 years"
dat_05to09 <- dat_05to09[,c("ISO3", "Year", "Age", v_othercol)]
v_othercol <- names(dat_10to14)[!(names(dat_10to14) %in% c("ISO3","Year", "AgeLow", "AgeUp","Model", "FragileState", "WHOname", "SDGregion","UNICEFReportRegion1", "UNICEFReportRegion2"))]
dat_10to14$Age <- "10 to 14 years"
dat_10to14 <- dat_10to14[,c("ISO3", "Year", "Age", v_othercol)]
v_othercol <- names(dat_15to19)[!(names(dat_15to19) %in% c("ISO3","Year", "AgeLow", "AgeUp","Model", "FragileState", "WHOname", "SDGregion","UNICEFReportRegion1", "UNICEFReportRegion2"))]
dat_15to19$Age <- "15 to 19 years"
dat_15to19 <- dat_15to19[,c("ISO3", "Year", "Age", v_othercol)]

# Recode Quantile values
dat_05to09$Quantile <- tolower(dat_05to09$Quantile)
dat_10to14$Quantile <- tolower(dat_10to14$Quantile)
dat_15to19$Quantile <- tolower(dat_15to19$Quantile)
dat_05to09$Quantile[dat_05to09$Quantile ==  "point"] <- "value"
dat_10to14$Quantile[dat_10to14$Quantile ==  "point"] <- "value"
dat_15to19$Quantile[dat_15to19$Quantile ==  "point"] <- "value"
dat_05to09$Quantile <- factor(dat_05to09$Quantile, levels = c("lower", "value", "upper"), ordered = TRUE)
dat_10to14$Quantile <- factor(dat_10to14$Quantile, levels = c("lower", "value", "upper"), ordered = TRUE)
dat_15to19$Quantile <- factor(dat_15to19$Quantile, levels = c("lower", "value", "upper"), ordered = TRUE)

# Order variable columns
dat_05to09$Variable <- factor(dat_05to09$Variable, levels = c("Deaths", "Fraction", "Rate"), ordered = TRUE)
dat_10to14$Variable <- factor(dat_10to14$Variable, levels = c("Deaths", "Fraction", "Rate"), ordered = TRUE)
dat_15to19$Variable <- factor(dat_15to19$Variable, levels = c("Deaths", "Fraction", "Rate"), ordered = TRUE)

# Sort columns if necessary
names(ui_5to9)
names(ui_10to14)
names(ui_15to19)
names(dat_05to09)
names(dat_10to14)
names(dat_15to19)

# Check number of rows
nrow(ui_5to9)   # 38610
nrow(ui_10to14) # 38610
nrow(ui_15to19) # 77220
nrow(dat_05to09) # 38610
nrow(dat_10to14) # 38610
nrow(dat_15to19) # 77220

# Sort rows
dat_05to09 <- dat_05to09[order(dat_05to09$ISO3, dat_05to09$Year, dat_05to09$Variable, dat_05to09$Quantile),]
dat_10to14 <- dat_10to14[order(dat_10to14$ISO3, dat_10to14$Year, dat_10to14$Variable, dat_10to14$Quantile),]
dat_15to19 <- dat_15to19[order(dat_15to19$ISO3, dat_15to19$Year, dat_15to19$Sex, dat_15to19$Variable, dat_15to19$Quantile),]

# Save and export
write.csv(dat_05to09, "./gen/adhoc-requests/output/Uncertainty5to9-National.csv", row.names = FALSE)
write.csv(dat_10to14, "./gen/adhoc-requests/output/Uncertainty10to14-National.csv", row.names = FALSE)
write.csv(dat_15to19, "./gen/adhoc-requests/output/Uncertainty15to19-National.csv", row.names = FALSE)

#####################################################################################
# Format aggregate age groups point and uncertainty estimates for Data Portal upload
#####################################################################################

#' Notes:
#' Pancho asked me to also calculate the aggregate age group files on 2023-11-30.
#' "This is my wish list, in order of importance:
#'    1.	10-19 FRACTIONS, with uncertainty, for all countries and regions. This is the top priority, as we need to incorporate the uncertainty to the pie charts of the country profiles https://childmortality.org/profiles
#'    2.	15-19 rates, deaths and fractions for BOTH SEXES combined, with uncertainty, for all countries and regions.
#'    3.	10-19 rates and deaths with uncertainty, for all countries and regions.
#'    4.	5-19 rates, deaths and fractions, with uncertainty, for all countries and regions."

# Regional ----------------------------------------------------------------

# Clear environment
rm(list = ls())

# Example files to match from Jamie and Pancho
ui_1to59REG <- read.csv("C:/Users/HEilerts/Dropbox/CA-CODE/CA-CODE_2021_SimpleUpdate/DataPortal/Uncertainty1to59m-Regional.csv")
ui_5to9 <- read.csv("./data/previous-results/2000-2021/Uncertainty5to9-National.csv")
ui_10to14 <- read.csv("./data/previous-results/2000-2021/Uncertainty10to14-National.csv")
ui_15to19 <- read.csv("./data/previous-results/2000-2021/Uncertainty15to19-National.csv")
csmf_5to9REG <- read.csv("./data/previous-results/2000-2021/PointEstimates5to9-Regional.csv")

# Load in my final results
final_resDate <- "20231201"
dat_05to19 <- read.csv(paste("./gen/aggregation/output/Uncertainty_Regional_05to19_", final_resDate, ".csv", sep = "")) 
dat_10to19 <- read.csv(paste("./gen/aggregation/output/Uncertainty_Regional_10to19_", final_resDate, ".csv", sep = "")) 
dat_15to19 <- read.csv(paste("./gen/aggregation/output/Uncertainty_Regional_15to19_", final_resDate, ".csv", sep = "")) 

# Recode sexes from "Both" to "Total"
dat_05to19$Sex <- "Total"
dat_10to19$Sex <- "Total"
dat_15to19$Sex <- "Total"

# Recode age
v_othercol <- names(dat_05to19)[!(names(dat_05to19) %in% c("Region","Year", "AgeLow", "AgeUp"))]
dat_05to19$Age <- "5 to 19 years"
dat_05to19 <- dat_05to19[,c("Region", "Year", "Age", v_othercol)]
v_othercol <- names(dat_10to19)[!(names(dat_10to19) %in% c("Region","Year", "AgeLow", "AgeUp"))]
dat_10to19$Age <- "10 to 19 years"
dat_10to19 <- dat_10to19[,c("Region", "Year", "Age", v_othercol)]
v_othercol <- names(dat_15to19)[!(names(dat_15to19) %in% c("Region","Year", "AgeLow", "AgeUp"))]
dat_15to19$Age <- "15 to 19 years"
dat_15to19 <- dat_15to19[,c("Region", "Year", "Age", v_othercol)]

# Recode Quantile values
dat_05to19$Quantile <- tolower(dat_05to19$Quantile)
dat_10to19$Quantile <- tolower(dat_10to19$Quantile)
dat_15to19$Quantile <- tolower(dat_15to19$Quantile)
dat_05to19$Quantile[dat_05to19$Quantile ==  "median"] <- "value"
dat_10to19$Quantile[dat_10to19$Quantile ==  "median"] <- "value"
dat_15to19$Quantile[dat_15to19$Quantile ==  "median"] <- "value"
dat_05to19$Quantile <- factor(dat_05to19$Quantile, levels = c("lower", "value", "upper"), ordered = TRUE)
dat_10to19$Quantile <- factor(dat_10to19$Quantile, levels = c("lower", "value", "upper"), ordered = TRUE)
dat_15to19$Quantile <- factor(dat_15to19$Quantile, levels = c("lower", "value", "upper"), ordered = TRUE)

# Order variable columns
dat_05to19$Variable <- factor(dat_05to19$Variable, levels = c("Deaths", "Fraction", "Rate"), ordered = TRUE)
dat_10to19$Variable <- factor(dat_10to19$Variable, levels = c("Deaths", "Fraction", "Rate"), ordered = TRUE)
dat_15to19$Variable <- factor(dat_15to19$Variable, levels = c("Deaths", "Fraction", "Rate"), ordered = TRUE)

# Match regional names in example files
sort(unique(ui_1to59REG$Region))
sort(unique(csmf_5to9REG$Region))
sort(unique(dat_05to19$Region))
unique(dat_05to19$Region)[!(unique(dat_05to19$Region) %in% unique(csmf_5to9REG$Region))]
unique(csmf_5to9REG$Region)[!(unique(csmf_5to9REG$Region) %in% unique(dat_05to19$Region))]
unique(ui_1to59REG$Region)[!(unique(ui_1to59REG$Region) %in% unique(dat_05to19$Region))]
# Drop "Europe and central Asia" "Sub-Saharan Africa" from my file
dat_05to19 <- subset(dat_05to19, !(Region %in% c("Europe and central Asia", "Sub-Saharan Africa")))
dat_10to19 <- subset(dat_10to19, !(Region %in% c("Europe and central Asia", "Sub-Saharan Africa")))
dat_15to19 <- subset(dat_15to19, !(Region %in% c("Europe and central Asia", "Sub-Saharan Africa")))
length(unique(dat_05to19$Region)) # 10
length(unique(dat_10to19$Region)) # 10
length(unique(dat_15to19$Region)) # 10
# Convert to ordered factor
v_regorder <- unique(csmf_5to9REG$Region)
dat_05to19$Region <- factor(dat_05to19$Region, levels = v_regorder, ordered = TRUE)
dat_10to19$Region <- factor(dat_10to19$Region, levels = v_regorder, ordered = TRUE)
dat_15to19$Region <- factor(dat_15to19$Region, levels = v_regorder, ordered = TRUE)
unique(dat_05to19$Region)
unique(dat_10to19$Region)
unique(dat_15to19$Region)

# Sort columns if necessary
names(csmf_5to9REG)
names(ui_5to9)
names(ui_10to14)
names(ui_15to19)
names(dat_05to19)
names(dat_10to19)
names(dat_15to19)

# Check number of rows
nrow(csmf_5to9REG) # 660
nrow(dat_05to19) # 1980 = 660*3
nrow(dat_10to19) # 1980
nrow(dat_15to19) # 1980

# Sort rows
dat_05to19 <- dat_05to19[order(dat_05to19$Region, dat_05to19$Year, dat_05to19$Variable, dat_05to19$Quantile),]
dat_10to19 <- dat_10to19[order(dat_10to19$Region, dat_10to19$Year, dat_10to19$Variable, dat_10to19$Quantile),]
dat_15to19 <- dat_15to19[order(dat_15to19$Region, dat_15to19$Year, dat_15to19$Variable, dat_15to19$Quantile),]

# Save and export
write.csv(dat_05to19, "./gen/adhoc-requests/output/Uncertainty5to19-Regional.csv", row.names = FALSE)
write.csv(dat_10to19, "./gen/adhoc-requests/output/Uncertainty10to19-Regional.csv", row.names = FALSE)
write.csv(dat_15to19, "./gen/adhoc-requests/output/Uncertainty15to19sexcombined-Regional.csv", row.names = FALSE)

# National ----------------------------------------------------------------

# Clear environment
rm(list = ls())

# Example files to match from Pancho
ui_5to9 <- read.csv("./data/previous-results/2000-2021/Uncertainty5to9-National.csv")
ui_10to14 <- read.csv("./data/previous-results/2000-2021/Uncertainty10to14-National.csv")
ui_15to19 <- read.csv("./data/previous-results/2000-2021/Uncertainty15to19-National.csv")
head(ui_5to9)
head(ui_10to14)
head(ui_15to19)

# Load in my final results
final_resDate <- "20231201"
dat_05to19 <- read.csv(paste("./gen/aggregation/output/Uncertainty_National_05to19_", final_resDate, ".csv", sep = "")) 
dat_10to19 <- read.csv(paste("./gen/aggregation/output/Uncertainty_National_10to19_", final_resDate, ".csv", sep = "")) 
dat_15to19 <- read.csv(paste("./gen/aggregation/output/Uncertainty_National_15to19_", final_resDate, ".csv", sep = "")) 

# Recode sexes from "Both" to "Total"
dat_05to19$Sex <- "Total"
dat_10to19$Sex <- "Total"
dat_15to19$Sex <- "Total"

# Recode age
v_othercol <- names(dat_05to19)[!(names(dat_05to19) %in% c("ISO3","Year", "AgeLow", "AgeUp", "Model", "FragileState", "WHOname", "SDGregion","UNICEFReportRegion1", "UNICEFReportRegion2"))]
dat_05to19$Age <- "5 to 19 years"
dat_05to19 <- dat_05to19[,c("ISO3", "Year", "Age", v_othercol)]
v_othercol <- names(dat_10to19)[!(names(dat_10to19) %in% c("ISO3","Year", "AgeLow", "AgeUp","Model", "FragileState", "WHOname", "SDGregion","UNICEFReportRegion1", "UNICEFReportRegion2"))]
dat_10to19$Age <- "10 to 19 years"
dat_10to19 <- dat_10to19[,c("ISO3", "Year", "Age", v_othercol)]
v_othercol <- names(dat_15to19)[!(names(dat_15to19) %in% c("ISO3","Year", "AgeLow", "AgeUp","Model", "FragileState", "WHOname", "SDGregion","UNICEFReportRegion1", "UNICEFReportRegion2"))]
dat_15to19$Age <- "15 to 19 years"
dat_15to19 <- dat_15to19[,c("ISO3", "Year", "Age", v_othercol)]

# Recode Quantile values
dat_05to19$Quantile <- tolower(dat_05to19$Quantile)
dat_10to19$Quantile <- tolower(dat_10to19$Quantile)
dat_15to19$Quantile <- tolower(dat_15to19$Quantile)
dat_05to19$Quantile[dat_05to19$Quantile ==  "median"] <- "value"
dat_10to19$Quantile[dat_10to19$Quantile ==  "median"] <- "value"
dat_15to19$Quantile[dat_15to19$Quantile ==  "median"] <- "value"
dat_05to19$Quantile <- factor(dat_05to19$Quantile, levels = c("lower", "value", "upper"), ordered = TRUE)
dat_10to19$Quantile <- factor(dat_10to19$Quantile, levels = c("lower", "value", "upper"), ordered = TRUE)
dat_15to19$Quantile <- factor(dat_15to19$Quantile, levels = c("lower", "value", "upper"), ordered = TRUE)

# Order variable columns
dat_05to19$Variable <- factor(dat_05to19$Variable, levels = c("Deaths", "Fraction", "Rate"), ordered = TRUE)
dat_10to19$Variable <- factor(dat_10to19$Variable, levels = c("Deaths", "Fraction", "Rate"), ordered = TRUE)
dat_15to19$Variable <- factor(dat_15to19$Variable, levels = c("Deaths", "Fraction", "Rate"), ordered = TRUE)

# Sort columns if necessary
names(ui_5to9)
names(ui_10to14)
names(ui_15to19)
names(dat_05to19)
names(dat_10to19)
names(dat_15to19)

# Check number of rows
nrow(ui_5to9)   # 38610
nrow(ui_10to14) # 38610
nrow(ui_15to19) # 77220
nrow(dat_05to19) # 38610
nrow(dat_10to19) # 38610
nrow(dat_15to19) # 38610

# Sort rows
dat_05to19 <- dat_05to19[order(dat_05to19$ISO3, dat_05to19$Year, dat_05to19$Variable, dat_05to19$Quantile),]
dat_10to19 <- dat_10to19[order(dat_10to19$ISO3, dat_10to19$Year, dat_10to19$Variable, dat_10to19$Quantile),]
dat_15to19 <- dat_15to19[order(dat_15to19$ISO3, dat_15to19$Year, dat_15to19$Variable, dat_15to19$Quantile),]

# Save and export
write.csv(dat_05to19, "./gen/adhoc-requests/output/Uncertainty5to19-National.csv", row.names = FALSE)
write.csv(dat_10to19, "./gen/adhoc-requests/output/Uncertainty10to19-National.csv", row.names = FALSE)
write.csv(dat_15to19, "./gen/adhoc-requests/output/Uncertainty15to19sexcombined-National.csv", row.names = FALSE)


