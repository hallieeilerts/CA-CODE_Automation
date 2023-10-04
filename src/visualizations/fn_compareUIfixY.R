fn_compareUIfixY <- function(DAT1, DAT2, CODALL, VARIABLE, REGIONAL = FALSE, SAMPLE = NULL){
  
  #' @title Plot point estimates and uncertainty intervals
  # 
  #' @description with fixed y axis instead of free
  #
  #' @param DAT1 Data frame 1 with all identifying columns and formatted point estimates, lower, and upper bounds for fractions/deaths/and rates
  #' @param DAT2 Data frame 2 with all identifying columns and formatted point estimates, lower, and upper bounds for fractions/deaths/and rates
  #' @param VARIABLE String with value "Deaths", "Fraction", "Rate"
  #' @param CODALL Vector with CODs for all age groups in correct order.
  #' @param REGIONAL Boolean with true/false value if regional estimates.
  #' @param SAMPLE Vector of sample of ISO3 codes to plot
  #' @return PDF plots with one country/region per page and facets for COD. Each facet is a line graph for the fraction/rate/deaths over the years being estimated, with a ribbon for the uncertainty interval.
  
  if(REGIONAL == FALSE){
    DAT2$name <- DAT2$ISO3
    DAT1$name <- DAT1$ISO3
  }else{
    DAT2$name <- DAT2$Region
    DAT1$name <- DAT1$Region
  }
  
  # Harmonize Sex names
  DAT1$Sex[DAT1$Sex == "T"] <- sexLabels[1]
  DAT1$Sex[DAT1$Sex == "B"] <- sexLabels[1]
  DAT1$Sex[DAT1$Sex == "F"] <- sexLabels[2]
  DAT1$Sex[DAT1$Sex == "M"] <- sexLabels[3]
  DAT1 <- subset(DAT1, Sex == sexLabel)
  DAT2$Sex[DAT2$Sex == "T"] <- sexLabels[1]
  DAT2$Sex[DAT2$Sex == "B"] <- sexLabels[1]
  DAT2$Sex[DAT2$Sex == "F"] <- sexLabels[2]
  DAT2$Sex[DAT2$Sex == "M"] <- sexLabels[3]
  DAT2 <- subset(DAT2, Sex == sexLabel)
  
  # Harmonize Quantile names
  # Add uncertainty columns if one of the sheets being compared doesn't have uncertainty intervals
  if("Quantile" %in% names(DAT1)){
    DAT1$Quantile[DAT1$Quantile == "lower"] <- "Lower"
    DAT1$Quantile[DAT1$Quantile == "upper"] <- "Upper"
    DAT1$Quantile[DAT1$Quantile == "value"] <- "Point"
  }else{
    DAT1$Quantile <- "Point"
    DAT1$Variable <- VARIABLE
  }
  if("Quantile" %in% names(DAT2)){
    DAT2$Quantile[DAT2$Quantile == "lower"] <- "Lower"
    DAT2$Quantile[DAT2$Quantile == "upper"] <- "Upper"
    DAT2$Quantile[DAT2$Quantile == "value"] <- "Point"
  }else{
    DAT2$Quantile <- "Point"
    DAT2$Variable <- VARIABLE
  }
  
  
  # Delete unnecessary columns
  DAT1 <- DAT1[-grep(c("ISO3|Region|FragileState|WHOname|SDGregion|UNICEFReportRegion1|UNICEFReportRegion2|Deaths|Rate|Qx|Model"), names(DAT1))]
  DAT2 <- DAT2[-grep(c("ISO3|Region|FragileState|WHOname|SDGregion|UNICEFReportRegion1|UNICEFReportRegion2|Deaths|Rate|Qx|Model"), names(DAT2))]
  
  # Subset deaths, rates, or fractions
  DAT1 <- subset(DAT1, Variable == VARIABLE)
  DAT2 <- subset(DAT2, Variable == VARIABLE)
  DAT1$Variable <- "Value"
  DAT2$Variable <- "Value"
  
  # Vector of CODs for reshaping
  v_cod <- CODALL[CODALL %in% names(DAT1)]
  
  # Reshape CODs long
  dat1Long <- melt(setDT(DAT1), measure.vars = v_cod, variable.name = "COD")
  # Reshape Variable and Quantile wide
  dat1Wide <- dcast(dat1Long, ... ~ Variable + Quantile, value.var = "value")
  
  dat2Long <- melt(setDT(DAT2), measure.vars = v_cod, variable.name = "COD")
  dat2Wide <- dcast(dat2Long, ... ~ Variable + Quantile, value.var = "value")
  
  dat1Wide$update <- "current"
  dat2Wide$update <- "other"
  
  dat <- bind_rows(dat1Wide, dat2Wide)
  dat$update <- factor(dat$update, levels = c("other", "current"))
  
  # Sample countries for national results
  if(length(SAMPLE) > 0){
    dat <- subset(dat, name %in% SAMPLE)
  }
  
  # PATCH ----------#
  # 2023-09-29
  # To match regions on data portal
  if(REGIONAL == TRUE){
    dat <- subset(dat, !(name %in% c("Europe and central Asia","Sub-Saharan Africa")))
  }
  # ---------------#
  
  # # To test single plot
  # dat1 <- subset(dat, name == "East Asia and Pacific")
  # ggplot(data = dat1) +
  #   geom_line(aes(x=Year, y=Value_Point, color = update, linetype = update), linewidth = 1) +
  #   geom_ribbon(aes(x=Year, ymin=Value_Lower, ymax = Value_Upper, fill = update, col = update), alpha = .3) +
  #   #labs(title = paste(x$name, VARIABLE, sep = " - "), subtitle = paste(x$AgeLow,"-",x$AgeUp,", ", x$Sex, sep = "")) +
  #   xlab("") + ylab("") +
  #   coord_cartesian(xlim = c(2000,2020)) +
  #   scale_x_continuous(breaks = c(2000, 2010, 2020)) +
  #   scale_color_manual(values = c("gray", "firebrick2")) +
  #   scale_fill_manual(values = c("gray", "firebrick2")) +
  #   scale_linetype_manual(values = c("solid", "longdash")) +
  #   facet_wrap(~COD, scales = "free") +
  #   theme_classic() +
  #   theme(panel.grid.major = element_blank(),
  #         panel.grid.minor = element_blank(),
  #         strip.background = element_blank(),
  #         panel.border = element_rect(colour = "black", fill = NA),
  #         plot.subtitle = element_text(hjust = 0),
  #         axis.text = element_text(size = 8))
  
  plots <- dlply(dat, ~name,
                 function(x)
                   ggplot(data = x) + 
                   geom_line(aes(x=Year, y=Value_Point, color = update, linetype = update), linewidth = 1) +
                   geom_ribbon(aes(x=Year, ymin=Value_Lower, ymax = Value_Upper, fill = update, col = update), alpha = .3) +
                   labs(title = paste(x$name, VARIABLE, sep = ", "), subtitle = paste(x$AgeLow,"-",x$AgeUp,", ", x$Sex, sep = "")) + 
                   xlab("") + ylab("") +
                   coord_cartesian(xlim = c(2000,2020), ylim = c(0,.4)) +
                   scale_x_continuous(breaks = c(2000, 2010, 2020)) +
                   scale_color_manual(values = c("gray", "firebrick2")) +
                   scale_fill_manual(values = c("gray", "firebrick2")) +
                   scale_linetype_manual(values = c("solid", "longdash")) +
                   facet_wrap(~COD) +
                   theme_classic() +
                   theme(panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank(),
                         strip.background = element_blank(),
                         panel.border = element_rect(colour = "black", fill = NA),
                         plot.subtitle = element_text(hjust = 0),
                         axis.text = element_text(size = 8)))
  mg <- marrangeGrob(grobs = plots, nrow=1, ncol=1, top = NULL)
  
  return(mg)
  
}
