
#-------------------#
# MINIMUM FRACTIONS #
#-------------------#
# See file 'Code/20201023_ReadTBdata.R'

# Minimum fraction of Other CD
if(ageGroup == "05to09"){frac_cd <- 0.0455}
if(ageGroup == "10to14"){frac_cd <- 0.0332}
if(ageGroup == "15to19f"){frac_cd <- 0.0474}
if(ageGroup == "15to19m"){frac_cd <- 0.0279}

# !!! can this be coded up?

###################################################################
######################### BEGIN-OUTPUTS ###########################
###################################################################

# Save output(s)
saveRDS(frac_cd, file = paste("./gen/squeezing/input/frac_cd_", ageGroup, ".rds", sep=""))
#save(frac_cd, file = paste("./gen/squeezing/input/frac_cd_", ageGroup, ".RData", sep=""))

###################################################################
######################### END-OUTPUTS #############################
###################################################################
