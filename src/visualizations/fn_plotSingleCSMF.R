fn_plotSingleCSMF <- function(DAT, VARIABLE, REGIONAL = FALSE, SAMPLE = NULL){
  
  #' @title Compare CSMFs
  # 
  #' @description Compare CSMFs for each cause to another set of results
  #
  #' @param DAT1 Data frame 1 with all identifying columns and formatted point estimates
  #' @param DAT2 Data frame 2 with all identifying columns and formatted point estimates
  #' @param REGIONAL Boolean with true/false value if regional estimates
  #' @param SAMPLE Vector of sample of ISO3 codes to plot
  #' @return PDF plots with one country/region per page and facets for COD. Each facet is a line graph for the CSMF over the years being estimated.
  
  dat <- DAT
  
  if(!REGIONAL){
    dat$name <- dat$ISO3
  }else{
    dat$name <- dat$Region
  }
  
  # Only keep median or point estimate
  dat <- subset(dat, Quantile %in% c("Median", "Point"))
  
  # Subset deaths, rates, or fractions
  dat <- subset(dat, Variable == VARIABLE)
  
  # Delete unnecessary columns
  dat <- dat[-grep(c("ISO3|Region|FragileState|WHOname|SDGregion|UNICEFReportRegion1|UNICEFReportRegion2|Deaths|Rate|Qx|Model|Quantile|Variable"), names(dat))]
  
  # Reshape mortality fractions into long format
  dat <- melt(setDT(dat), id.vars = c("name","AgeLow","AgeUp", "Sex","Year"))
  
  # Sample countries for national results
  if(length(SAMPLE) > 0){
    dat <- subset(dat, name %in% SAMPLE)
  }
  
  # To test single plot
  # dat1 <- subset(dat, name == "AFG")
  # ggplot(data = dat1) +
  #   geom_line(aes(x=Year, y=value), linewidth = 1) +
  #   #labs(title = x$name, subtitle = paste(x$AgeLow,"-",x$AgeUp,", ", x$Sex, sep = "")) + 
  #   xlab("") + ylab("") +
  #   coord_cartesian(xlim = c(2000,2020), ylim = c(0,.8)) +
  #   scale_x_continuous(breaks = c(2000, 2010, 2020)) +
  #   facet_wrap(~variable) +
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
                   geom_line(aes(x=Year, y=value), linewidth = 1) +
                   labs(title = x$name, subtitle = paste(x$AgeLow,"-",x$AgeUp,", ", x$Sex, sep = "")) + 
                   xlab("") + ylab("") +
                   coord_cartesian(xlim = c(2000,2020), ylim = c(0,.8)) +
                   scale_x_continuous(breaks = c(2000, 2010, 2020)) +
                   facet_wrap(~variable) +
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
