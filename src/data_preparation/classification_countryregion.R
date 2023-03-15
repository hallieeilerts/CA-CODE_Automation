

# COUNTRY CLASSIFICATION
countryClass <- read.csv('./data/simple_update_2021/classification_keys/20201001-CountryModelClass.csv')

# SDG REGIONAL CLASSIFICATION
regions <- read.dta13('./data/simple_update_2021/classification_keys/20190528-RegionClassSDG.dta', nonint.factors = T)
regions <- regions[, names(regions) %in% c('dimensionmembercode', 'whoname', 'sdg1')]

# Re-label regions
regions$sdg1[regions$sdg1 == 'Australia and New Zealand (M49)'] <- 'Australia and New Zealand'
regions$sdg1[regions$sdg1 == 'Central Asia (M49) and Southern Asia (MDG=M49)'] <- 'Central and Southern Asia'
regions$sdg1[regions$sdg1 == 'Eastern Asia (M49) and South-eastern Asia (MDG=M49)'] <- 'Eastern and South-eastern Asia'
regions$sdg1[regions$sdg1 == 'Latin America & the Caribbean (MDG=M49)'] <- 'Latin America and the Caribbean'
regions$sdg1[regions$sdg1 == 'Northern America (M49) and Europe (M49)'] <- 'Northern America and Europe'
regions$sdg1[regions$sdg1 == 'Oceania (MDG) / Oceania (M49) excluding Australia and New Zealand (M49)'] <- 'Oceania'
regions$sdg1[regions$sdg1 == 'Sub-Saharan Africa (M49)'] <- 'Sub-Saharan Africa'
regions$sdg1[which(!regions$sdg1 %in% c('Australia and New Zealand', 'Central and Southern Asia',
                                        'Eastern and South-eastern Asia', 'Latin America and the Caribbean',
                                        'Northern America and Europe', 'Oceania',
                                        'Sub-Saharan Africa', ''))] <- 'Western Asia and Northern Africa'
table(regions$sdg1)

# Re-label variables
names(regions)[names(regions) == 'dimensionmembercode'] <- idVars[1]
names(regions)[names(regions) == 'whoname'] <- 'WHOname'
names(regions)[names(regions) == 'sdg1'] <- 'SDGregion'
head(regions)

# IGME REGIONS
regIGME <- read.csv('./data/simple_update_2021/classification_keys/20210407-RegionClassIGME.csv')
regIGME <- regIGME[, names(regIGME) %in% c('ISO3Code', 'UNICEFReportRegion1', 'UNICEFReportRegion2')]
regIGME$UNICEFReportRegion1[regIGME$UNICEFReportRegion1 == 'Europe and Central Asia'] <- 'Europe and central Asia'
regIGME$UNICEFReportRegion2[regIGME$UNICEFReportRegion2 == 'West and Central Africa'] <- 'West and central Africa'
regIGME$UNICEFReportRegion2[regIGME$UNICEFReportRegion2 == 'Eastern Europe and Central Asia'] <- 'Eastern Europe and central Asia'
regions <- merge(regions, regIGME, by.x = 'ISO3', by.y = 1)
rm(regIGME)
head(regions)



