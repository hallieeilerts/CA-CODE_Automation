
#-------------------#
# MINIMUM FRACTIONS #
#-------------------#
# See file 'Code/20201023_ReadTBdata.R'


# Minimum fraction of LRI
if(ageGroup == "05to09"){minLRI <- 0.0269}
if(ageGroup == "10to14"){minLRI <- 0.0197}
if(ageGroup %in% c("15to19f","15to19m")){minLRI <- NULL}

# !!! can this be coded up?

###################################################################
######################### BEGIN-OUTPUTS ###########################
###################################################################

# Save output(s)
saveRDS(minLRI, file = paste("./gen/squeezing/input/minfrac_lri_", ageGroup, ".rds", sep=""))
#save(minLRI, file = paste("./gen/squeezing/input/minfrac_lri_", ageGroup, ".RData", sep=""))

###################################################################
######################### END-OUTPUTS #############################
###################################################################
