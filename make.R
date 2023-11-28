
################################################
# CA CODE Automation
################################################

# Clear environment
rm(list = ls())

source("./src/prepare-session/prepare-session_master.R")

source("./src/data-management/data-management_master.R")

source("./src/estimation/estimation_master.R")

source("./src/prediction/prediction_master.R")

source("./src/squeezing/squeezing_master.R")

source("./src/uncertainty/uncertainty_master.R")

source("./src/results/results_master.R")