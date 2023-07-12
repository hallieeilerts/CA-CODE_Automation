################################################################################
#' @description Manually sets age group and years being estimated.
#' @return Strings, booleans, integers, date defined below
################################################################################

## Type of update
simpleUpdate <- TRUE

## Choose age/sex group
# ageGroup <- "00to28"
# ageGroup <- "01to04"
ageGroup <- "05to09"
# ageGroup <- "10to14"
# ageGroup <- "15to19f"
# ageGroup <- "15to19m"

## Set years for update
Years <- 2000:2021

## Results date (for naming output files)
resDate <- format(Sys.Date(), format="%Y%m%d")
# resDate <- "20230602" # Can set manually
