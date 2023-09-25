
#-------------------#
# MINIMUM FRACTIONS #
#-------------------#
# See file 'Code/20201023_ReadTBdata.R'


# Minimum fraction of LRI
if(ageGroup == "05to09"){frac_lri <- 0.0269}
if(ageGroup == "10to14"){frac_lri <- 0.0197}
if(ageGroup %in% c("15to19f","15to19m")){frac_lri <- NULL}

# !!! can this be coded up?

###################################################################
######################### BEGIN-OUTPUTS ###########################
###################################################################

# Save output(s)
saveRDS(frac_lri, file = paste("./gen/squeezing/input/frac_lri_", ageGroup, ".rds", sep=""))
#save(frac_lri, file = paste("./gen/squeezing/input/frac_lri_", ageGroup, ".RData", sep=""))

###################################################################
######################### END-OUTPUTS #############################
###################################################################
