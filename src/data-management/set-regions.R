################################################################################
## @Purpose - Sets regional classifications for countries.
## @Date - 2023-06-27
## @Author - Hallie Eilerts-Spinelli
################################################################################

# Clear environment
rm(list = ls())

# Install packages and load libraries

# Load inputs
key_region_u20_who  <- read.dta13("./data/classification-keys/20190528-RegionClassSDG.dta", nonint.factors = T)
key_region_u20_igme <- read.csv("./data/classification-keys/20210407-RegionClassIGME.csv")

################################################################################


#-----------------------------#
# SDG REGIONAL CLASSIFICATION #
#-----------------------------#

dat1 <- key_region_u20_who
dat1 <- dat1[, names(dat1) %in% c("dimensionmembercode", "whoname", "sdg1")]
# Re-label dat1
dat1$sdg1[dat1$sdg1 == "Australia and New Zealand (M49)"] <- "Australia and New Zealand"
dat1$sdg1[dat1$sdg1 == "Central Asia (M49) and Southern Asia (MDG=M49)"] <- "Central and Southern Asia"
dat1$sdg1[dat1$sdg1 == "Eastern Asia (M49) and South-eastern Asia (MDG=M49)"] <- "Eastern and South-eastern Asia"
dat1$sdg1[dat1$sdg1 == "Latin America & the Caribbean (MDG=M49)"] <- "Latin America and the Caribbean"
dat1$sdg1[dat1$sdg1 == "Northern America (M49) and Europe (M49)"] <- "Northern America and Europe"
dat1$sdg1[dat1$sdg1 == "Oceania (MDG) / Oceania (M49) excluding Australia and New Zealand (M49)"] <- "Oceania"
dat1$sdg1[dat1$sdg1 == "Sub-Saharan Africa (M49)"] <- "Sub-Saharan Africa"
dat1$sdg1[which(!dat1$sdg1 %in% c("Australia and New Zealand", "Central and Southern Asia",
                                        "Eastern and South-eastern Asia", "Latin America and the Caribbean",
                                        "Northern America and Europe", "Oceania",
                                        "Sub-Saharan Africa", ""))] <- "Western Asia and Northern Africa"
# Re-label variables
names(dat1)[names(dat1) == "dimensionmembercode"] <- idVars[1]
names(dat1)[names(dat1) == "whoname"] <- "WHOname"
names(dat1)[names(dat1) == "sdg1"] <- "SDGregion"
head(dat1)

#-----------------------------#
# IGME REGION CLASSIFICATION  #
#-----------------------------#

dat2 <- key_region_u20_igme
dat2 <- dat2[, names(dat2) %in% c("ISO3Code", "UNICEFReportRegion1", "UNICEFReportRegion2")]
dat2$UNICEFReportRegion1[dat2$UNICEFReportRegion1 == "Europe and Central Asia"] <- "Europe and central Asia"
dat2$UNICEFReportRegion2[dat2$UNICEFReportRegion2 == "West and Central Africa"] <- "West and central Africa"
dat2$UNICEFReportRegion2[dat2$UNICEFReportRegion2 == "Eastern Europe and Central Asia"] <- "Eastern Europe and central Asia"

#-------#
# MERGE #
#-------#

dat <- merge(dat1, dat2, by.x = "ISO3", by.y = 1)

# Remove unnecessary objects
rm(dat1, dat2, key_region_u20_who, key_region_u20_igme)

###################################################################
######################### BEGIN-OUTPUTS ###########################
###################################################################

# Save output(s)
write.csv(dat, "./gen/data-prep/output/key_region_u20.csv", row.names = FALSE)

###################################################################
######################### END-OUTPUTS #############################
###################################################################

