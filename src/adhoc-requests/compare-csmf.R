
################################################################
# Stand-alone file for producing line plots of CSMF comparisons
################################################################

#' Notes:
#' Requested by Jamie October 17, 2023 for the Geneva meeting

# Clear environment
rm(list = ls())

library(ggplot2)
library(gridExtra)
library(data.table) # melt()
library(plyr) # dlply()

# Set age group -----------------------------------------------------------

ageGroup <- "05to09"
# ageGroup <- "10to14"
# ageGroup <- "15to19f"
# ageGroup <- "15to19m"

# Load data ---------------------------------------------------------------

# Final estimates from current round
pointInt <- read.csv(paste("./gen/results/output/Uncertainty_National_", ageGroup, "_20231002.csv", sep = ""))
pointInt_REG <- read.csv(paste("./gen/results/output/Uncertainty_Regional_", ageGroup, "_20231002.csv", sep = ""))

# Pancho's estimates from previous estimation round
if(ageGroup == "05to09"){point_PrevResults <- read.csv("./data/previous-results/2000-2019/PointEstimates5to9-National.csv")
                         point_PrevResults_REG <- read.csv("./data/previous-results/2000-2019/PointEstimates5to9-Regional.csv")}
if(ageGroup == "10to14"){point_PrevResults <- read.csv("./data/previous-results/2000-2019/PointEstimates10to14-National.csv")
                         point_PrevResults_REG <- read.csv("./data/previous-results/2000-2019/PointEstimates10to14-Regional.csv")}
if(ageGroup %in% c("15to19f", "15to19m")){point_PrevResults <- read.csv("./data/previous-results/2000-2019/PointEstimates15to19-National.csv")
                                          point_PrevResults_REG <- read.csv("./data/previous-results/2000-2019/PointEstimates15to19-Regional.csv")}

# Harmonize format --------------------------------------------------------

DAT1 <- pointInt
DAT1_REG <- pointInt_REG
DAT2 <- point_PrevResults
DAT2_REG <- point_PrevResults_REG

DAT1$update <- "2021"
DAT1_REG$update <- "2021"
DAT2$update <- "2019"
DAT2_REG$update <- "2019"

DAT1$name <- DAT1$WHOname
DAT1_REG$name <- DAT1_REG$Region
DAT2$name <- DAT2$WHOname
DAT2_REG$name <- DAT2_REG$Region

DAT1$AdminLevel <- "National"
DAT1_REG$AdminLevel <- "Regional"
DAT2$AdminLevel <- "National"
DAT2_REG$AdminLevel <- "Regional"

# Subset fractions for pointInt
DAT1 <- subset(DAT1, Variable == "Fraction" & Quantile == "Point")
DAT1_REG <- subset(DAT1_REG, Variable == "Fraction" & Quantile == "Point")

# Delete unnecessary columns
DAT1 <- DAT1[-grep(c("ISO3|Region|FragileState|WHOname|SDGregion|UNICEFReportRegion1|UNICEFReportRegion2|Deaths|Rate|Qx|Model|Variable|Quantile"), names(DAT1))]
DAT1_REG <- DAT1_REG[-grep(c("ISO3|Region|FragileState|WHOname|SDGregion|UNICEFReportRegion1|UNICEFReportRegion2|Deaths|Rate|Qx|Model|Variable|Quantile"), names(DAT1_REG))]
DAT2 <- DAT2[-grep(c("ISO3|Region|FragileState|WHOname|SDGregion|UNICEFReportRegion1|UNICEFReportRegion2|Deaths|Rate|Qx|Model|Variable|Quantile"), names(DAT2))]
DAT2_REG <- DAT2_REG[-grep(c("ISO3|Region|FragileState|WHOname|SDGregion|UNICEFReportRegion1|UNICEFReportRegion2|Deaths|Rate|Qx|Model|Variable|Quantile"), names(DAT2_REG))]

# Harmonize Sex names
sexLabels <- c("Both", "Female", "Male") 
DAT1$Sex[DAT1$Sex == "Total"] <- sexLabels[1]
DAT1$Sex[DAT1$Sex == "T"] <- sexLabels[1]
DAT1$Sex[DAT1$Sex == "B"] <- sexLabels[1]
DAT1$Sex[DAT1$Sex == "F"] <- sexLabels[2]
DAT1$Sex[DAT1$Sex == "M"] <- sexLabels[3]
DAT1_REG$Sex[DAT1_REG$Sex == "Total"] <- sexLabels[1]
DAT1_REG$Sex[DAT1_REG$Sex == "T"] <- sexLabels[1]
DAT1_REG$Sex[DAT1_REG$Sex == "B"] <- sexLabels[1]
DAT1_REG$Sex[DAT1_REG$Sex == "F"] <- sexLabels[2]
DAT1_REG$Sex[DAT1_REG$Sex == "M"] <- sexLabels[3]
DAT2$Sex[DAT2$Sex == "Total"] <- sexLabels[1]
DAT2$Sex[DAT2$Sex == "T"] <- sexLabels[1]
DAT2$Sex[DAT2$Sex == "B"] <- sexLabels[1]
DAT2$Sex[DAT2$Sex == "F"] <- sexLabels[2]
DAT2$Sex[DAT2$Sex == "M"] <- sexLabels[3]
DAT2_REG$Sex[DAT2_REG$Sex == "Total"] <- sexLabels[1]
DAT2_REG$Sex[DAT2_REG$Sex == "T"] <- sexLabels[1]
DAT2_REG$Sex[DAT2_REG$Sex == "B"] <- sexLabels[1]
DAT2_REG$Sex[DAT2_REG$Sex == "F"] <- sexLabels[2]
DAT2_REG$Sex[DAT2_REG$Sex == "M"] <- sexLabels[3]

# Only keep males or females from Pancho's if ageGroup is 15-19
if(ageGroup == "15to19m"){
  DAT2 <- subset(DAT2, Sex == sexLabels[3])
  DAT2_REG <- subset(DAT2_REG, Sex == sexLabels[3])
}
if(ageGroup == "15to19f"){
  DAT2 <- subset(DAT2, Sex == sexLabels[2])
  DAT2_REG <- subset(DAT2_REG, Sex == sexLabels[2])
}

# Rbind all together
dat <- rbind(DAT1, DAT1_REG, DAT2, DAT2_REG)

# Reshape to long
dat <- melt(setDT(dat), id.vars = c("update","AdminLevel","name","AgeLow","AgeUp", "Sex","Year"))

# Names not included in 2019
dat <- subset(dat, !(name %in% c("Sub-Saharan Africa","Europe and central Asia")))

# Set factor levels and order
v_NameOrder <- c("World",
                 unique(subset(dat, AdminLevel == "Regional")$name)[which(unique(subset(dat, AdminLevel == "Regional")$name) != "World")],
                 unique(subset(dat, AdminLevel == "National")$name))
dat$name <- factor(dat$name, levels = v_NameOrder, ordered = TRUE)
dat <- dat[order(dat$name),]
dat$update <- factor(dat$update, levels = c("2021","2019"))


# To test single plot
# dattest <- subset(dat, name == "Afghanistan")
# ggplot(dattest) +
#   geom_line(aes(x=Year, y=value, color = update), linewidth = 1) +
#   labs(subtitle = dattest$name) + xlab("") + ylab("") +
#   coord_cartesian(xlim = c(2000,2020), ylim = c(0,.8)) +
#   scale_x_continuous(breaks = c(2000,2010, 2020)) +
#   scale_color_manual(values = c("2019"="green", "2021"="purple")) +
#   facet_wrap(~variable, scales = "free") +
#   theme_classic() +
#   theme(legend.title=element_blank(),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         strip.background = element_blank(),
#         panel.border = element_rect(colour = "black", fill = NA),
#         plot.subtitle = element_text(hjust = 0),
#         axis.text = element_text(size = 8))

plots <- dlply(subset(dat, name %in% c("Afghanistan", "Burkina Faso")), ~name,
               function(x)
                 ggplot(data = x) + 
                 geom_line(aes(x=Year, y=value, color = update), linewidth = 1) +
                 labs(subtitle = x$name) + xlab("") + ylab("") +
                 coord_cartesian(xlim = c(2000,2020), ylim = c(0,.8)) +
                 scale_x_continuous(breaks = c(2000,2010, 2020)) +
                 scale_color_manual(values = c("2019"="green", "2021"="purple")) +
                 facet_wrap(~variable, scales = "free") +
                 theme_classic() +
                 theme(legend.title=element_blank(),
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       strip.background = element_blank(),
                       panel.border = element_rect(colour = "black", fill = NA),
                       plot.subtitle = element_text(hjust = 0),
                       axis.text = element_text(size = 8)))


mg <- marrangeGrob(grobs = plots, nrow=1, ncol=1, top = NULL)

ggsave(paste("./gen/visualizations/output/Compare_estimates_", ageGroup,"_", format(Sys.Date(), format="%Y%m%d"), ".pdf", sep=""), mg, height = 10, width = 8, units = "in")
