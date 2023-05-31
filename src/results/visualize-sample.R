
###################################################################
########################## BEGIN-INPUTS ###########################
###################################################################

# Load packages and session variables if not already loaded
if(!exists("sessionVars")){source("./src/prepare-session.R")
  load("./gen/data-prep/input/session-variables.Rdata")}

# Load input(s)
if(ageGroup == "05to09"){dat <- read.csv(paste("./gen/results/output/PointEstimates_National_05to09_", resDate, ".csv", sep =""))
                         datOLD <- read.csv("./data/previous-results/2000-2021/PointEstimates5to9-National.csv")}
if(ageGroup == "10to14"){dat <-  read.csv(paste("./gen/results/output/PointEstimates_National_10to14_", resDate, ".csv", sep =""))
                         datOLD <- read.csv("./data/previous-results/2000-2021/PointEstimates10to14-National.csv")}
if(ageGroup == "15to19f"){dat <- read.csv(paste("./gen/results/output/PointEstimates_National_15to19f_", resDate, ".csv", sep =""))
                          datOLD <- read.csv("./data/previous-results/2000-2021/PointEstimates15to19-National.csv")}
if(ageGroup == "15to19m"){dat <-  read.csv(paste("./gen/results/output/PointEstimates_National_15to19m_", resDate, ".csv", sep =""))
                          datOLD <- read.csv("./data/previous-results/2000-2021/PointEstimates15to19-National.csv")}

###################################################################
########################## END-INPUTS #############################
###################################################################

if(sexSplit){
  datOLD$Sex[datOLD$Sex == "B"] <- sexLabels[1]
  datOLD$Sex[datOLD$Sex == "F"] <- sexLabels[2]
  datOLD$Sex[datOLD$Sex == "M"] <- sexLabels[3]
  datOLD <- subset(datOLD, Sex == sexLabel)
}

dat$update <- "new"
dat$AgeLow <- ageLow
dat$AgeUp <- ageUp
datOLD$update <- "old"
names(datOLD)[which(names(datOLD) == "Qx")] <- "Rate" 

dat <- bind_rows(dat, datOLD)
dat$update <- factor(dat$update, levels = c("old", "new"))

# Delete unnecessary columns
dat <- dat[-grep(c("FragileState|WHOname|SDGregion|UNICEFReportRegion1|UNICEFReportRegion2|Deaths|Rate|Model"), names(dat))]

# Reshape mortality fractions into long format
dat <- melt(setDT(dat), id.vars = c("update","ISO3","AgeLow","AgeUp", "Sex","Year"))

# Order plots alphabetically by world region and then nation
v_ctries <- c("AFG", "BRA", "CHN", "IND", "MEX", "NGA", "SDN")
dat <- subset(dat, ISO3 %in% v_ctries)
dat <- dat[order(dat$ISO3),]
plots <- dlply(dat, ~ISO3,
               function(x)
                 ggplot(data = x) + 
                 geom_line(aes(x=Year, y=value, color = update, linetype = update), linewidth = 1) +
                 labs(title = x$ISO3, subtitle = paste(x$AgeLow,"-",x$AgeUp,", ", x$Sex, sep = "")) + 
                 xlab("") + ylab("") +
                 coord_cartesian(xlim = c(2000,2020), ylim = c(0,.8)) +
                 scale_x_continuous(breaks = c(2000, 2010, 2020)) +
                 scale_color_manual(values = c("gray", "firebrick2")) +
                 scale_linetype_manual(values = c("solid", "longdash")) +
                 facet_wrap(~variable, scales = "free") +
                 theme_classic() +
                 theme(panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       strip.background = element_blank(),
                       panel.border = element_rect(colour = "black", fill = NA),
                       plot.subtitle = element_text(hjust = 0),
                       axis.text = element_text(size = 8)))
mg <- marrangeGrob(grobs = plots, nrow=1, ncol=1, top = NULL)


###################################################################
######################### BEGIN-OUTPUTS ###########################
###################################################################

# Save output(s)
ggsave(paste("./gen/results/temp/compare_sample_", ageGroup, ".pdf", sep=""), mg, height = 10, width = 8, units = "in")

###################################################################
######################### END-OUTPUTS #############################
###################################################################
