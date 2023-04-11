
######################
#
# Set inputs
#
######################

## Choose age/sex group
agegrp <- "00to01"
# agegrp <- "01to59"
# agegrp <- "05to09"
# agegrp <- "10to14"
# agegrp <- "15to19f"
# agegrp <- "15to19m"

################################################
# Prediction
################################################

################################################
# Squeezing
################################################

if(agegrp == "00to01"){
  source(src/squeezing/splt_sepsis.R)
  source(src/squeezing/scl_crisisEnd.R)
  source(src/squeezing/sqz_allEnd.R)
  source(src/squeezing/scl_crisisEpi.R)
  source(src/squeezing/sqz_crisisEpi.R)
}
if(agegrp == "01to59"){
  source(src/squeezing/splt_peri.R)
  source(src/squeezing/posthocAdj_vac.R)
  source(src/squeezing/adj_tb.R)
  source(src/squeezing/splt_ocmpn.R) # and adjust for minimum threshold
  source(src/squeezing/splt_lri.R)       # and adjust for minimum threshold
  source(src/squeezing/scl_crisisEnd.R)
  source(src/squeezing/sqz_allEnd.R)    # squeezing all end (to all)
  source(src/squeezing/scl_crisisEpi.R)
  source(src/squeezing/sqz_crisisEpi.R) # squeezing in epi crisis (to all)
  source(src/squeezing/sqz_measEpi.R)   # squeezing in epi meas (to all)
}
if(agegrp == "05to09"){
  source(src/squeezing/splt_ocmpn.R)
  source(src/squeezing/splt_lri.R)
  source(src/squeezing/scl_crisisEnd.R)
  source(src/squeezing/sqz_allEnd.R)
  source(src/squeezing/scl_crisisEpi.R)
  source(src/squeezing/sqz_crisisEpi.R)
  source(src/squeezing/sqz_measEpi.R)
}
if(agegrp == "10to14"){
  source(src/squeezing/splt_ocmpn.R)
  source(src/squeezing/splt_lri.R)
  source(src/squeezing/scl_crisisEnd.R)
  source(src/squeezing/sqz_allEnd.R)
  source(src/squeezing/scl_crisisEpi.R)
  source(src/squeezing/sqz_crisisEpi.R)
}
if(agegrp %in% c("15to19f", "15to19m")){
  source(src/squeezing/splt_ocmpn.R)
  source(src/squeezing/scl_crisisEnd.R)
  source(src/squeezing/sqz_allEnd.R)
  source(src/squeezing/scl_crisisEpi.R)
  source(src/squeezing/sqz_crisisEpi.R)
}



