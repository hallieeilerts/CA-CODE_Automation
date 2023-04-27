
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

source(src/prediction/predict-csmf.R)

################################################
# Squeezing
################################################

source(src/squeezing/prep-squeezing.R)

if(agegrp == "00to01"){
  source(src/squeezing/split-sepsis.R)
}
if(agegrp == "01to59"){
  source(src/squeezing/split-perinatal.R)         # split perinatal into preterm and intrapartum
  source(src/squeezing/adjust-vaccine.R)          # posthoc vaccine adjustment
  source(src/squeezing/adjust-tb-underrec.R)      # reassigns excess pulmonary tb as carve out of lri
  source(src/squeezing/squeeze-othercmpn.R)       # squeezing single causes into othercmpn
  source(src/squeezing/squeeze-crisis-epidemic.R) # squeezing in epi crisis
  source(src/squeezing/add-measles-epidemic.R)    # squeezing in epi meas
}
if(agegrp == "05to09"){
  source(src/squeezing/squeeze-othercmpn.R)       # and adjust for minimum threshold
  source(src/squeezing/squeeze-lri.R)             # and adjust for minimum threshold
  source(src/squeezing/squeeze-crisis-endemic.R)  # cap endemic crisis at max GHE fraction, squeeze with all endemic causes
  source(src/squeezing/squeeze-crisis-epidemic.R)
  source(src/squeezing/add-measles-epidemic.R)
}
if(agegrp == "10to14"){
  source(src/squeezing/squeeze-ocmpn.R)
  source(src/squeezing/squeeze-lri.R)
  source(src/squeezing/squeeze-crisis-endemic.R)
  source(src/squeezing/add-crisis-epidemic.R)
}
if(agegrp %in% c("15to19f", "15to19m")){
  source(src/squeezing/squeeze-ocmpn.R)
  source(src/squeezing/squeeze-crisis-endemic.R)
  source(src/squeezing/squeeze-crisis-epidemic.R)
}

source(src/squeezing/format-squeezed-output.R)

################################################
# Uncertainty
################################################

source(src/uncertainty/calculate-uncertainty.R)


