
#################
#               #
# SIMPLE UPDATE #
#               #
#################


##############
# Set inputs
##############

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

source(src/squeezing/reshapeForSqueezing.R)

if(agegrp == "00to01"){
  source(src/squeezing/splt_sepsis.R)
  source(src/squeezing/sqz_crisisEnd.R)
  source(src/squeezing/sqz_crisisEpi.R)
}
if(agegrp == "01to59"){
  source(src/squeezing/splt_peri.R)      # split perinatal into preterm and intrapartum
  source(src/squeezing/adj_vac.R)        # posthoc vaccine adjustment
  source(src/squeezing/adj_tbUnderrec.R) # reassigns excess pulmonary tb as carve out of lri
  source(src/squeezing/sqz_ocmpn.R)      # squeezing single causes into othercmpn
  source(src/squeezing/sqz_crisisEnd.R)  # cap endemic crisis at max GHE fraction, squeeze with all endemic causes
  source(src/squeezing/sqz_crisisEpi.R)  # squeezing in epi crisis
  source(src/squeezing/add_measEpi.R)    # squeezing in epi meas
}
if(agegrp == "05to09"){
  source(src/squeezing/sqz_ocmpn.R) # and adjust for minimum threshold
  source(src/squeezing/sqz_lri.R)   # and adjust for minimum threshold
  source(src/squeezing/sqz_crisisEnd.R)
  source(src/squeezing/sqz_crisisEpi.R)
  source(src/squeezing/add_measEpi.R)
}
if(agegrp == "10to14"){
  source(src/squeezing/sqz_ocmpn.R)
  source(src/squeezing/sqz_lri.R)
  source(src/squeezing/sqz_crisisEnd.R)
  source(src/squeezing/add_crisisEpi.R)
}
if(agegrp %in% c("15to19f", "15to19m")){
  source(src/squeezing/sqz_ocmpn.R)
  source(src/squeezing/sqz_crisisEnd.R)
  source(src/squeezing/sqz_crisisEpi.R)
}

################################################
# Uncertainty
################################################


