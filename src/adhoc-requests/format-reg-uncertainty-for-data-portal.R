##############################################################
# Format regional uncertainty files for Data Portal upload
##############################################################

#' Notes:
#' I offered to calculate regional uncertainty files on 2023-09-28 since I had already gotten Pancho's code working for this.
#' My results need to be formatted to match his and Jamie's.
#' There are very few differences, just a few related to capitalization of strings or naming of regions.
#' The region names are different because they have to match what UN IGME uses on the Data Portal.

# Example files to match from Jamie and Pancho
ui_1to59REG <- read.csv("C:/Users/HEilerts/Dropbox/CA-CODE/CA-CODE_2021_SimpleUpdate/DataPortal/Uncertainty1to59m-Regional.csv")
ui_5to9 <- read.csv("C:/Users/HEilerts/Dropbox/CA-CODE/Mort5to19/Results/2000-2021/Uncertainty5to9-National.csv")
ui_10to14 <- read.csv("C:/Users/HEilerts/Dropbox/CA-CODE/Mort5to19/Results/2000-2021/Uncertainty10to14-National.csv")
ui_15to19 <- read.csv("C:/Users/HEilerts/Dropbox/CA-CODE/Mort5to19/Results/2000-2021/Uncertainty15to19-National.csv")
csmf_5to9REG <- read.csv("C:/Users/HEilerts/Dropbox/CA-CODE/Mort5to19/Results/2000-2021/PointEstimates5to9-Regional.csv")

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
write.csv(dat_05to09, "./gen/adhoc-requests/Uncertainty5to9-Regional.csv", row.names = FALSE)
write.csv(dat_10to14, "./gen/adhoc-requests/Uncertainty10to14-Regional.csv", row.names = FALSE)
write.csv(dat_15to19, "./gen/adhoc-requests/Uncertainty15to19-Regional.csv", row.names = FALSE)


