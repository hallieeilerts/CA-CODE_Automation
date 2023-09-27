fn_plotSingleUI <- function(DAT, CODALL, VARIABLE, REGIONAL = FALSE, SAMPLE = NULL){
  
  #' @title Plot point estimates and uncertainty intervals
  # 
  #' @description 
  #
  #' @param DAT1 Data frame 1 with all identifying columns and formatted point estimates, lower, and upper bounds for fractions/deaths/and rates
  #' @param DAT2 Data frame 2 with all identifying columns and formatted point estimates, lower, and upper bounds for fractions/deaths/and rates
  #' @param VARIABLE String with value "Deaths", "Fraction", "Rate"
  #' @param CODALL Vector with CODs for all age groups in correct order.
  #' @param REGIONAL Boolean with true/false value if regional estimates.
  #' @param SAMPLE Vector of sample of ISO3 codes to plot
  #' @return PDF plots with one country/region per page and facets for COD. Each facet is a line graph for the fraction/rate/deaths over the years being estimated, with a ribbon for the uncertainty interval.
  
  if(!REGIONAL){
    DAT$name <- DAT$ISO3
  }else{
    DAT$name <- DAT$Region
  }
  
  # Delete unnecessary columns
  DAT <- DAT[-grep(c("ISO3|Region|FragileState|WHOname|SDGregion|UNICEFReportRegion1|UNICEFReportRegion2|Deaths|Rate|Qx|Model"), names(DAT))]
  
  # Subset deaths, rates, or fractions
  DAT <- subset(DAT, Variable == VARIABLE)
  DAT$Variable <- "Value"
  
  # Rename median as point
  DAT$Quantile[DAT$Quantile == "Median"] <- "Point"
  
  # Vector of CODs for reshaping
  v_cod <- CODALL[CODALL %in% names(DAT)]
  
  # Reshape CODs long
  datLong <- melt(setDT(DAT), measure.vars = v_cod, variable.name = "COD")
  # Reshape Variable and Quantile wide
  datWide <- dcast(datLong, ... ~ Variable + Quantile, value.var = "value")
  
  # Data for plotting
  dat <- datWide
  
  # Sample countries for national results
  if(length(SAMPLE) > 0){
    dat <- subset(dat, name %in% SAMPLE)
  }
  
  # To test single plot
  # dat <- subset(dat, name == "AFG")
  # ggplot(data = dat) +
  #   geom_line(aes(x=Year, y=Value_Point), linewidth = 1) +
  #   geom_ribbon(aes(x=Year, ymin=Value_Lower, ymax = Value_Upper), alpha = .3) +
  #   xlab("") + ylab("") +
  #   coord_cartesian(xlim = c(2000,2020)) +
  #   scale_x_continuous(breaks = c(2000, 2010, 2020)) +
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
                   geom_line(aes(x=Year, y=Value_Point), linewidth = 1) +
                   geom_ribbon(aes(x=Year, ymin=Value_Lower, ymax = Value_Upper), alpha = .3) +
                   labs(title = paste(x$name, VARIABLE, sep = ", "), subtitle = paste(x$AgeLow,"-",x$AgeUp,", ", x$Sex, sep = "")) + 
                   xlab("") + ylab("") +
                   coord_cartesian(xlim = c(2000,2020)) +
                   scale_x_continuous(breaks = c(2000, 2010, 2020)) +
                   facet_wrap(~COD, scales = "free") +
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
