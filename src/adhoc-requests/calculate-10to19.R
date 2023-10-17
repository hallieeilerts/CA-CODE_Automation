##############################################################
# Calculate national and regional estimates for 10-19
##############################################################

#' Notes:
#' Pancho asked me to calculate the national uncertainty files on 2023-10-17 since he has not had time to re-do them with Jamie's most up-to-date results file (child_cod_2000-2021.dta). 
#' The one he used was missing Cote d'Ivoire, so malaria was capped at zero for 5-9 and 10-14.
#' Updating these also meant I needed to update the national and regional 10-19 point estimates.
#' I could calculate 10-19 with my code, but I will use his, since it works on the formatted results files that I produced in adhoc-requests/format-for-data-portal
#' My calculations are performed on earlier versions of the files (that are not rounded).
#' His code also only reports deaths, which is apparently all we need for the country profiles on the data portal.

library(plyr) # dlply
library(data.table) # melt
library(ggplot2)
library(gridExtra)

# Clear environment
rm(list = ls())

############################################################
### NATIONAL ESTIMATES                                   ###  
############################################################

# 10-14 COUNTRY ESTIMATES
dat10to14 <- read.csv('./gen/adhoc-requests/output/Uncertainty10to14-National.csv')
dat10to14 <- dat10to14[dat10to14$Variable == 'Deaths', ]
dat10to14 <- dat10to14[dat10to14$Quantile == 'value', ]

# 15-19 COUNTRY ESTIMATES
dat15to19 <- read.csv('./gen/adhoc-requests/output/Uncertainty15to19-National.csv')
dat15to19 <- dat15to19[dat15to19$Variable == 'Deaths', ]
dat15to19 <- dat15to19[dat15to19$Quantile == 'value', ]

# Add missing columns
addCol <- names(dat15to19)[which(!names(dat15to19) %in% names(dat10to14))]
dat10to14[, paste(addCol)] <- 0
addCol <- names(dat10to14)[which(!names(dat10to14) %in% names(dat15to19))]
dat15to19[, paste(addCol)] <- 0

# COLLAPSE
dat <- rbind(dat10to14, dat15to19)
dat$Age <- '10 to 19 years'
dat$Sex <- 'Total'
dat <- aggregate(dat[, 7:ncol(dat)],
                 by = list(dat$Year, dat$ISO3, dat$Age, dat$Sex, 
                           dat$Variable, dat$Quantile),
                 FUN = sum)
dat <- dat[, c(2, 1, 3:ncol(dat))]
names(dat)[1:6] <- names(dat10to14)[1:6]

# Save output
write.csv(dat, row.names = F, file = './gen/adhoc-requests/output/PointEstimates10to19-National.csv')

############################################################
# Plot to compare national estimates with Pancho's
############################################################

# Should be the same except for some differences with malaria countries

datPancho <- read.csv("./data/previous-results/2000-2021/PointEstimates10to19-National.csv")
dat$update <- "Hal"
datPancho$update <- "Pancho"
datPlot <- rbind(dat, datPancho)
datPlot <- melt(setDT(datPlot), id.vars = c("update","ISO3","Age","Sex","Year","Variable","Quantile"))

plots <- dlply(datPlot, ~ISO3,
               function(x)
                 ggplot(data = x) + 
                 geom_line(aes(x=Year, y=value, color = update, linetype = update), linewidth = 1) +
                 labs(title = x$ISO3, subtitle = paste(x$Age)) + 
                 xlab("") + ylab("") +
                 coord_cartesian(xlim = c(2000,2020)) +
                 scale_x_continuous(breaks = c(2000, 2010, 2020)) +
                 scale_color_manual(values = c("gray", "firebrick2")) +
                 scale_linetype_manual(values = c("solid", "longdash")) +
                 facet_wrap(~variable) +
                 theme_classic() +
                 theme(panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       strip.background = element_blank(),
                       panel.border = element_rect(colour = "black", fill = NA),
                       plot.subtitle = element_text(hjust = 0),
                       axis.text = element_text(size = 8)))
mg <- marrangeGrob(grobs = plots, nrow=1, ncol=1, top = NULL)
ggsave(paste("./gen/adhoc-requests/audit/csmf_comparison_national_10to19_", format(Sys.Date(), format="%Y%m%d"), ".pdf", sep=""), mg, height = 10, width = 8, units = "in")

#' Notes:
#' See malaria differences in CAF, CIV
#' I see rounding differences in FSM and GRD. Not going to worry about them.
#' Small difference for SOM OtherCMPN and Diarrhoeal. Not sure why.

# Remove objects
rm(addCol, dat, dat10to14, dat15to19, datPancho)


############################################################
### REGIONAL ESTIMATES                                   ###  
############################################################

# Clear environment
rm(list = ls())

# 10-14 REGIONAL ESTIMATES
dat10to14 <- read.csv('./gen/adhoc-requests/output/Uncertainty10to14-Regional.csv')
dat10to14 <- dat10to14[dat10to14$Variable == 'Deaths', ]
dat10to14 <- dat10to14[dat10to14$Quantile == 'value', ]

# 15-19 COUNTRY ESTIMATES
dat15to19 <- read.csv('./gen/adhoc-requests/output/Uncertainty15to19-Regional.csv')
dat15to19 <- dat15to19[dat15to19$Variable == 'Deaths', ]
dat15to19 <- dat15to19[dat15to19$Quantile == 'value', ]

# Add missing columns
addCol <- names(dat15to19)[which(!names(dat15to19) %in% names(dat10to14))]
dat10to14[, paste(addCol)] <- 0
addCol <- names(dat10to14)[which(!names(dat10to14) %in% names(dat15to19))]
dat15to19[, paste(addCol)] <- 0

# Combine data frames
dat <- rbind(dat10to14, dat15to19)
dat$Age <- '10 to 19 years'
dat$Sex <- 'Total'

# Collapse
dat <- aggregate(dat[, 7:ncol(dat)],
                 by = list(dat$Year, dat$Region, dat$Age, dat$Sex, 
                           dat$Variable, dat$Quantile),
                 FUN = sum)
dat[, c(7:ncol(dat))] <- round(dat[, c(7:ncol(dat))])
dat <- dat[, c(2, 1, 3:ncol(dat))]
names(dat)[1:6] <- names(dat10to14)[1:6]

# Put World first
dat <- rbind(dat[dat$Region == 'World', ],
             dat[dat$Region != 'World', ])
rownames(dat) <- NULL

# Save output
write.csv(dat, row.names = F, file = './gen/adhoc-requests/output/PointEstimates10to19-Regional.csv')


############################################################
# Plot to compare regional estimates with Pancho's
############################################################

# Should be the same except for some differences due to difference malaria caps for 10-14

datPancho <- read.csv("./data/previous-results/2000-2021/PointEstimates10to19-Regional.csv")
dat$update <- "Hal"
datPancho$update <- "Pancho"
datPlot <- rbind(dat, datPancho)
datPlot <- melt(setDT(datPlot), id.vars = c("update","Region","Age","Sex","Year","Variable","Quantile"))

plots <- dlply(datPlot, ~Region,
               function(x)
                 ggplot(data = x) + 
                 geom_line(aes(x=Year, y=value, color = update, linetype = update), linewidth = 1) +
                 labs(title = x$Region, subtitle = paste(x$Age)) + 
                 xlab("") + ylab("") +
                 coord_cartesian(xlim = c(2000,2020)) +
                 scale_x_continuous(breaks = c(2000, 2010, 2020)) +
                 scale_color_manual(values = c("gray", "firebrick2")) +
                 scale_linetype_manual(values = c("solid", "longdash")) +
                 facet_wrap(~variable) +
                 theme_classic() +
                 theme(panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       strip.background = element_blank(),
                       panel.border = element_rect(colour = "black", fill = NA),
                       plot.subtitle = element_text(hjust = 0),
                       axis.text = element_text(size = 8)))
mg <- marrangeGrob(grobs = plots, nrow=1, ncol=1, top = NULL)
ggsave(paste("./gen/adhoc-requests/audit/csmf_comparison_regional_10to19_", format(Sys.Date(), format="%Y%m%d"), ".pdf", sep=""), mg, height = 10, width = 8, units = "in")

# Remove objects
rm(addCol, dat, dat10to14, dat15to19, datPancho)




