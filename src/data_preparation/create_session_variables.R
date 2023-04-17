
# Helper script to edit Rdata file containing with session variables

# Session variables are simple strings that are called in multiple other scripts.
# Instead of defining them in each script, we define them once here and save as an RData file.

# This R script is not included in "make".
# Run only if the code is changed, and adjustments to the session variables are needed.

# Variables that identify records
idVars <- c('ISO3', 'Year', 'Sex')

save(idVars, file = "./simple_update_2021/gen/data_preparation/input/session_variables.RData")