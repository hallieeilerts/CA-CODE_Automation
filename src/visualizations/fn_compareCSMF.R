fn_compareCSMF <- function(DAT1, DAT2, REGIONAL = FALSE, SAMPLE = NULL){
  
  #' @title Compare CSMFs
  # 
  #' @description Compare CSMFs for each cause to another set of results
  #
  #' @param DAT1 Data frame 1 with all identifying columns and formatted point estimates
  #' @param DAT2 Data frame 2 with all identifying columns and formatted point estimates
  #' @param REGIONAL Boolean with true/false value if regional estimates
  #' @param SAMPLE Vector of sample of ISO3 codes to plot
  #' @return PDF plots with one country/region per page and facets for COD. Each facet is a line graph for the CSMF over the years being estimated.
  
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
  
  DAT1$update <- "current"
  DAT2$update <- "other"
  
  dat <- bind_rows(DAT1, DAT2)
  dat$update <- factor(dat$update, levels = c("other", "current"))
  
  # Delete unnecessary columns
  dat <- dat[-grep(c("ISO3|Region|FragileState|WHOname|SDGregion|UNICEFReportRegion1|UNICEFReportRegion2|Deaths|Rate|Qx|Model"), names(dat))]
  
  # Reshape mortality fractions into long format
  dat <- melt(setDT(dat), id.vars = c("update","name","AgeLow","AgeUp", "Sex","Year"))
  
  # Sample countries for national results
  if(length(SAMPLE) > 0){
    dat <- subset(dat, name %in% SAMPLE)
  }
  
  # Order plots alphabetically by world region and then nation
  
  plots <- dlply(dat, ~name,
                 function(x)
                   ggplot(data = x) + 
                   geom_line(aes(x=Year, y=value, color = update, linetype = update), linewidth = 1) +
                   labs(title = x$name, subtitle = paste(x$AgeLow,"-",x$AgeUp,", ", x$Sex, sep = "")) + 
                   xlab("") + ylab("") +
                   coord_cartesian(xlim = c(2000,2020), ylim = c(0,.8)) +
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
  
  return(mg)
  
}
