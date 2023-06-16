
#-------------------#
# MINIMUM FRACTIONS #
#-------------------#
# See file 'Code/20201023_ReadTBdata.R'

# Minimum fraction of Other CD
if(ageGroup == "05to09"){minCD <- 0.0455}
if(ageGroup == "10to14"){minCD <- 0.0332}
if(ageGroup == "15to19f"){minCD <- 0.0474}
if(ageGroup == "15to19m"){minCD <- 0.0279}

# !!! can this be coded up?

###################################################################
######################### BEGIN-OUTPUTS ###########################
###################################################################

# Save output(s)
saveRDS(minCD, file = paste("./gen/squeezing/input/minfrac_cd_", ageGroup, ".rds", sep=""))
#save(minCD, file = paste("./gen/squeezing/input/minfrac_cd_", ageGroup, ".RData", sep=""))

###################################################################
######################### END-OUTPUTS #############################
###################################################################
