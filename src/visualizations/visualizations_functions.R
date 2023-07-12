
##################################################
####
####   Compare CSMFs for each cause to results of previous year
####
##################################################

fn_compare_csmf <- function(dat, dat_Old, regional = FALSE, sample = NULL){
  
  if(regional == FALSE){
    dat_Old$name <- dat_Old$ISO3
    dat$name <- dat$ISO3
  }else{
    dat_Old$name <- dat_Old$Region
    dat$name <- dat$Region
  }

  if(sexSplit){
    dat_Old$Sex[dat_Old$Sex == "B"] <- sexLabels[1]
    dat_Old$Sex[dat_Old$Sex == "F"] <- sexLabels[2]
    dat_Old$Sex[dat_Old$Sex == "M"] <- sexLabels[3]
    dat_Old <- subset(dat_Old, Sex == sexLabel)
  }
  
  dat$update <- "new"
  dat_Old$update <- "old"

  dat <- bind_rows(dat, dat_Old)
  dat$update <- factor(dat$update, levels = c("old", "new"))
  
  # Delete unnecessary columns
  dat <- dat[-grep(c("ISO3|Region|FragileState|WHOname|SDGregion|UNICEFReportRegion1|UNICEFReportRegion2|Deaths|Rate|Qx|Model"), names(dat))]
  
  # Reshape mortality fractions into long format
  dat <- melt(setDT(dat), id.vars = c("update","name","AgeLow","AgeUp", "Sex","Year"))
  
  # Sample countries for national results
  if(length(sample) > 0){
    dat <- subset(dat, name %in% sample)
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
                   facet_wrap(~variable, scales = "free") +
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



