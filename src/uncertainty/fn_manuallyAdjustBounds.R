fn_manuallyAdjustBounds <- function(POINTINT, REGIONAL = FALSE){
  
  #' @title Manually adjust country/year/cause-specific upper or lower bounds
  # 
  #' @description 2023.06.08 PATCH that manually adjusts some country/year/cause-specific confidence intervals.
  #
  #' @param POINTINT Data frame with rounded point estimates, lower, and upper bounds for fractions/deaths/rates
  #' @param REGIONAL Boolean with true/false value if regional estimates.
  #' @return Data frame with rounded point estimates, lower, and upper bounds for fractions/deaths/rates that have been adjusted for inconsistencies.
  
  dat <- POINTINT

  if(!REGIONAL){
    #------------------------#
    # 2023.06.08 PATCH
    # Pancho's adjustments
    # For rates, multiply by 100, floor() to remove decimals, divide by 100
    # if(ageGroup == "05to09"){
    #   # PANCHO's adjustments commented out
    #   # Measles in Turkey
    #   dat$Measles[which(dat$ISO3 == "TUR" & dat$Year %in% c(2003, 2008, 2009) &
    #                       dat$Variable == "Rate" & dat$Quantile == "Upper")] <-
    #     dat$Measles[which(dat$ISO3 == "TUR" & dat$Year %in% c(2003, 2008, 2009) &
    #                         dat$Variable == "Rate" & dat$Quantile == "Upper")] + 0.00002
    #   # HIV in Bangladesh
    #   dat$HIV[which(dat$ISO3 == "BGD" & dat$Year %in% c(2007) &
    #                   dat$Variable == "Rate" & dat$Quantile == "Upper")] <-
    #     dat$HIV[which(dat$ISO3 == "BGD" & dat$Year %in% c(2007) &
    #                     dat$Variable == "Rate" & dat$Quantile == "Upper")] + 0.00003
    # }
    if(ageGroup == "10to14"){
      # # Pancho's adjustments
      # # Somalia Nat Disasters in 2011
      # # dat$NatDis[which(dat$ISO3 == "SOM" & dat$Year == 2011 &
      # #                    dat$Variable == "Rate" & dat$Quantile == "Lower")] <-
      # #   floor(100*dat$NatDis[which(dat$ISO3 == "SOM" & dat$Year == 2011 &
      # #                       dat$Variable == "Rate" & dat$Quantile == "Lower")]) / 100
      # # My adjustments (no longer necessary)
      # # Micronesia (FSM) OtherCMPN in 2002
      # dat$OtherCMPN[which(dat$ISO3 == "FSM" & dat$Year == 2002 &
      #                          dat$Variable == "Deaths" & dat$Quantile == "Upper")] <-
      #   dat$OtherCMPN[which(dat$ISO3 == "FSM" & dat$Year == 2002 &
      #                            dat$Variable == "Deaths" & dat$Quantile == "Upper")] + 1
      # # Somalia HIV in 2010
      # dat$HIV[which(dat$ISO3 == "SOM" & dat$Year == 2010 &
      #                    dat$Variable == "Rate" & dat$Quantile == "Lower")] <-
      #   floor(100*dat$HIV[which(dat$ISO3 == "SOM" & dat$Year == 2010 &
      #                                dat$Variable == "Rate" & dat$Quantile == "Lower")]) / 100
    }
    if(ageGroup == "15to19f"){
      # Ukrainian females CollectVio in 2014 (both Pancho's and mine)
      dat[which(dat$ISO3 == "UKR" & dat$Year == 2014 &
                     dat$Variable == "Fraction" & dat$Quantile == "Upper"), "CollectVio"] <-
        dat[which(dat$ISO3 == "UKR" & dat$Year == 2014 &
                       dat$Variable == "Fraction" & dat$Quantile == "Upper"), "CollectVio"] + 0.0001
      # Jamaica females HIV in 2012
      dat$HIV[which(dat$ISO3 == "JAM" & dat$Year == 2012 &
                         dat$Variable == "Rate" & dat$Quantile == "Upper")] <-
        dat$HIV[which(dat$ISO3 == "JAM" & dat$Year == 2012 &
                           dat$Variable == "Rate" & dat$Quantile == "Upper")] + 0.001
      
    }
    if(ageGroup == "15to19m"){
      #   # Pancho's
      #   # Syrian males 2014
      #   dat[which(dat$ISO3 == "SYR" & dat$Year == 2014 &
      #               dat$Variable == "Rate" & dat$Quantile == "Upper"), "InterpVio"] <-
      #     dat[which(dat$ISO3 == "SYR" & dat$Year == 2014 &
      #                 dat$Variable == "Rate" & dat$Quantile == "Upper"), "InterpVio"] + .05
      #
      #   # Syrian males 2019
      #   dat[which(dat$ISO3 == "SYR" & dat$Year == 2019 &
      #               dat$Variable == "Rate" & dat$Quantile == "Upper"), c("OtherNCD", "RTI", "OtherInj")] <-
      #     dat[which(dat$ISO3 == "SYR" & dat$Year == 2019 &
      #                 dat$Variable == "Rate" & dat$Quantile == "Upper"), c("OtherNCD", "RTI", "OtherInj")] + 0.005
      #
      #   # Somoan males 2009
      #   dat[which(dat$ISO3 == "WSM" & dat$Year == 2009 &
      #               dat$Variable == "Rate" & dat$Quantile == "Upper"), "NatDis"] <-
      #     dat[which(dat$ISO3 == "WSM" & dat$Year == 2009 &
      #                 dat$Variable == "Rate" & dat$Quantile == "Upper"), "NatDis"] + .1
      #   # Tajikistan males 2000
      #   dat[which(dat$ISO3 == "TJK" & dat$Year == 2000 &
      #               dat$Variable == "Rate" & dat$Quantile == "Upper"), "CollectVio"] <-
      #     dat[which(dat$ISO3 == "TJK" & dat$Year == 2000 &
      #                 dat$Variable == "Rate" & dat$Quantile == "Upper"), "CollectVio"] + 0.003
      # My adjustments
      # Trinidad and Tobago males HIV 2008 and 2009
      dat[which(dat$ISO3 == "TTO" & dat$Year == 2008 &
                     dat$Variable == "Rate" & dat$Quantile == "Upper"), "HIV"] <-
        dat[which(dat$ISO3 == "TTO" & dat$Year == 2008 &
                       dat$Variable == "Rate" & dat$Quantile == "Upper"), "HIV"] + 0.0005
      dat[which(dat$ISO3 == "TTO" & dat$Year == 2009 &
                     dat$Variable == "Rate" & dat$Quantile == "Upper"), "HIV"] <-
        dat[which(dat$ISO3 == "TTO" & dat$Year == 2009 &
                       dat$Variable == "Rate" & dat$Quantile == "Upper"), "HIV"] + 0.0005
    }
    # END PATCH
    #------------------------#
  }
  
  return(dat)
}

