
# Helper script to edit Rdata with session variables
# Not included in make script

# Variables that identify records
idVars <- c('ISO3', 'Year', 'Sex')

save(idVars, file = "./simple_update_2021/gen/data_preparation/input/session_variables.RData")