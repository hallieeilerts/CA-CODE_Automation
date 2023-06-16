

###################################################################
########################## BEGIN-INPUTS ###########################
###################################################################

# Load packages and session variables if not already loaded
if(!exists("sessionVars")){source("./src/prepare-session.R")
  load("./gen/data-prep/input/session-variables.Rdata")}

# Load input(s)
nat05to09 <- read.csv(paste("./gen/results/output/PointEstimates_National_05to09_", resDate, ".csv", sep =""))
nat10to14 <- read.csv(paste("./gen/results/output/PointEstimates_National_10to14_", resDate, ".csv", sep =""))
nat15to19f <- read.csv(paste("./gen/results/output/PointEstimates_National_15to19f_", resDate, ".csv", sep =""))                     
nat15to19m <- read.csv(paste("./gen/results/output/PointEstimates_National_15to19m_", resDate, ".csv", sep =""))
#reg05to09 <- read.csv(paste("./gen/results/output/PointEstimates_Regional_05to09_", resDate, ".csv", sep =""))
#reg10to14 <- read.csv(paste("./gen/results/output/PointEstimates_Regional_10to14_", resDate, ".csv", sep =""))
#reg15to19f <- read.csv(paste("./gen/results/output/PointEstimates_Regional_15to19f_", resDate, ".csv", sep =""))                     
#reg15to19m <- read.csv(paste("./gen/results/output/PointEstimates_Regional_15to19m_", resDate, ".csv", sep =""))     

env <- read.csv("./gen/data-prep/output/env_crisisincl_u20.csv")
key_region <- read.csv("./gen/data-prep/output/key_region_u20.csv")
key_ctryclass <- read.csv("./gen/data-prep/output/key_ctryclass_u20.csv")

###################################################################
########################## END-INPUTS #############################
###################################################################

# Calculate aggregate rates
nat05to14 <- fn_calc_agg_rate(5,14, dat05to09 = nat05to09, dat10to14 = nat10to14)
nat05to19 <- fn_calc_agg_rate(5,19, env, dat05to09 = nat05to09, dat10to14 = nat10to14, dat15to19f = nat15to19f, dat15to19m = nat15to19m)
nat10to19 <- fn_calc_agg_rate(10,19, env, dat10to14 = nat10to14, dat15to19f = nat15to19f, dat15to19m = nat15to19m)
nat15to19 <- fn_calc_agg_rate(15,19, env, dat15to19f = nat15to19f, dat15to19m = nat15to19m)

# Format
nat05to14 <- fn_format_results(nat05to14, key_region, key_ctryclass, codAll)
nat05to19 <- fn_format_results(nat05to19, key_region, key_ctryclass, codAll)
nat10to19 <- fn_format_results(nat10to19, key_region, key_ctryclass, codAll)
nat15to19 <- fn_format_results(nat15to19, key_region, key_ctryclass, codAll)

# Calculate aggregate rates
#reg05to14 <- fn_calc_agg_rate(5,14, dat05to09 = reg05to09, dat10to14 = reg10to14)
#reg05to19 <- fn_calc_agg_rate(5,19, env, dat05to09 = reg05to09, dat10to14 = reg10to14, dat15to19f = reg15to19f, dat15to19m = reg15to19m)
#reg10to19 <- fn_calc_agg_rate(10,19, env, dat10to14 = reg10to14, dat15to19f = reg15to19f, dat15to19m = reg15to19m)
#reg15to19 <- fn_calc_agg_rate(15,19, env, dat15to19f = reg15to19f, dat15to19m = reg15to19m)

# Format
reg05to14 <- fn_format_results(reg05to14, key_region, key_ctryclass, codAll)
reg05to19 <- fn_format_results(reg05to19, key_region, key_ctryclass, codAll)
reg10to19 <- fn_format_results(reg10to19, key_region, key_ctryclass, codAll)
reg15to19 <- fn_format_results(reg15to19, key_region, key_ctryclass, codAll)

###################################################################
######################### BEGIN-OUTPUTS ###########################
###################################################################

# Save output(s)
#write.csv(nat05to14, paste("./gen/results/output/PointEstimates_National_05to14_", format(Sys.Date(), format="%Y%m%d"),".csv", sep=""), row.names = FALSE)
#write.csv(nat05to19, paste("./gen/results/output/PointEstimates_National_05to19_", format(Sys.Date(), format="%Y%m%d"),".csv", sep=""), row.names = FALSE)
#write.csv(nat10to19, paste("./gen/results/output/PointEstimates_National_10to19_", format(Sys.Date(), format="%Y%m%d"),".csv", sep=""), row.names = FALSE)
#write.csv(nat15to19, paste("./gen/results/output/PointEstimates_National_15to19_", format(Sys.Date(), format="%Y%m%d"),".csv", sep=""), row.names = FALSE)

###################################################################
######################### END-OUTPUTS #############################
###################################################################
