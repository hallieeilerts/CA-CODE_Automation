################################################################################
#' @description Set country class based on U5M
#' @return Data frame with c("ISO3", "Group", "Group2010", "FragileState")
################################################################################
#' Libraries
#' Inputs
dat <- read.csv("./data/classification-keys/20201001-CountryModelClass.csv")
################################################################################

# Add code that assigns countries to GOODVR, LMM or HMM
# env_crisisfree_u20_igme <- read_excel("./data/envelopes/UN IGME 2022 Rates & Deaths_Country Summary (crisis free) 1980-2021 all ages.xlsx")
# Should we be using crisis-free or crisis-included u5m rate in 2010 to set country class?
# Need to load information on whether its a fragile state. Where does this come from?

# As a place holder, I will just re-save 20201001-CountryModelClass.csv
# and use the updated name for this object (key_ctryclass_u20)

# Save output(s) ----------------------------------------------------------

write.csv(dat, "./gen/data-prep/output/key_ctryclass_u20.csv", row.names = FALSE)
