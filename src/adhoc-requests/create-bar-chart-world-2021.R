##############################################################
# Bar chart for global cause distribution for 2021
##############################################################

#' Notes:
#' Requested by Jamie October 17, 2023 for the Geneva meeting

# Clear environment
rm(list = ls())

library(ggplot2)
library(gridExtra)
library(dplyr) # bind_rows()
library(scales) # show_col(), hue_pal()

# Load data
dat05to09 <- read.csv(paste("./gen/results/output/Uncertainty_Regional_05to09_20231002.csv", sep = ""))
dat10to14 <- read.csv(paste("./gen/results/output/Uncertainty_Regional_10to14_20231002.csv", sep = ""))
dat15to19f <- read.csv(paste("./gen/results/output/Uncertainty_Regional_15to19f_20231002.csv", sep = ""))
dat15to19m <- read.csv(paste("./gen/results/output/Uncertainty_Regional_15to19m_20231002.csv", sep = ""))
# Vector with COD in correct order
codAll <- c("Measles", "Maternal", "HIV", "LRI",  "TB", "Diarrhoeal", "Malaria", "OtherCMPN",
            "Congenital", "Cardiovascular", "Digestive", "Neoplasms", "OtherNCD",
            "InterpVio","SelfHarm", "Drowning", "RTI", "OtherInj", "NatDis", "CollectVio")   


# Combine all
dat <- bind_rows(dat05to09, dat10to14, dat15to19f, dat15to19m)

# Only keep CSMFs and point estimates
dat <- subset(dat, Variable == "Fraction" & Quantile == "Point")
dat$Variable <- dat$Quantile <- NULL

# Only keep Global 2021
dat <- subset(dat, Year == 2021 & Region == "World")

# Fill in NAs
dat[is.na(dat)] <- 0

# Create new age column
dat$AgeGrp <- paste(dat$AgeLow, "-", dat$AgeUp, sep = "")
dat$AgeGrp[dat$Sex == "Female"] <- "15-19f"
dat$AgeGrp[dat$Sex == "Male"] <- "15-19m"

# Order age as factor
dat$AgeGrp <- factor(dat$AgeGrp, levels = c("5-9", "10-14", "15-19f", "15-19m"), ordered = TRUE)
dat$AgeLow <- dat$AgeUp <- dat$Sex <- NULL

# Reshape to long
dat <- melt(setDT(dat), id.vars = c("Region", "Year", "AgeGrp"))

# Order CODs as factor
dat$variable <- factor(dat$variable, levels = codAll, ordered = TRUE)

plot <- ggplot(dat) +
  geom_bar(aes(x=AgeGrp, y=value, fill = variable), stat = "identity") +
  labs(subtitle = "Global Fractions, 2021") + xlab("") + ylab("") +
  scale_fill_manual(values = hue_pal()(length(codAll))) +
  theme_classic() +
  theme(legend.position = "bottom",
        legend.title=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        plot.subtitle = element_text(hjust = 0))

ggsave(paste("./gen/adhoc-requests/output/GlobalFrac_2021_20231017.pdf", sep=""), plot, height = 6, width = 8, units = "in")

