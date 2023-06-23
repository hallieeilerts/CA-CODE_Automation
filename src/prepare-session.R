###################################################################
# Install and load required packages
###################################################################

# install pacman to streamline further package installation
if (!require("pacman", character.only = TRUE)){
  install.packages("pacman", dep = TRUE)
  if (!require("pacman", character.only = TRUE))
    stop("Package not found")
}

# these are the required packages
pkgs <- c(
  "readxl",
  "readstata13",
  "dplyr",
  "plyr",
  "tidyverse",
  "data.table",
  "ggplot2",
  "gridExtra",
  "msm" # Truncated normal distribution
)

# install the missing packages
# only run if at least one package is missing
if(!sum(!p_isinstalled(pkgs))==0){
  p_install(
    package = pkgs[!p_isinstalled(pkgs)], 
    character.only = TRUE
  )
}

# load the packages
p_load(pkgs, character.only = TRUE)
rm(pkgs)
