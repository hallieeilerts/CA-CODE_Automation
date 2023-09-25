################################################################################
#' @description Runs functions required for calculating CSMFs and uncertainty for aggregate age groups
#' @return Aggregate age group results
################################################################################

# Calculate CSMFs for aggregate age group (need to have already produced CSMFs for all standard age groups)
csmfSqz_AGG_05to14 <- fn_calc_agg_ages(AGELB = 5, AGEUB = 14, CODALL = codAll, CSMF_5TO9 = csmfSqz_05to09,  CSMF_10TO14 = csmfSqz_10to14)
csmfSqz_AGG_05to19 <- fn_calc_agg_ages(AGELB = 5, AGEUB = 19, CODALL = codAll, CSMF_5TO9 = csmfSqz_05to09,  CSMF_10TO14 = csmfSqz_10to14, CSMF_15TO19F = csmfSqz_15to19f, CSMF_15TO19M = csmfSqz_15to19m, ENV = env_u20)
csmfSqz_AGG_10to19 <- fn_calc_agg_ages(AGELB = 10, AGEUB = 19, CODALL = codAll, CSMF_10TO14 = csmfSqz_10to14, CSMF_15TO19F = csmfSqz_15to19f, CSMF_15TO19M = csmfSqz_15to19m, ENV = env_u20)
write.csv(csmfSqz_AGG_05to14, paste("./gen/aggregation/output/csmfSqz_AGG_05to14.csv", sep = ""))
write.csv(csmfSqz_AGG_05to19, paste("./gen/aggregation/output/csmfSqz_AGG_05to19.csv", sep = ""))
write.csv(csmfSqz_AGG_10to19, paste("./gen/aggregation/output/csmfSqz_AGG_10to19.csv", sep = ""))

# Sample from envelope draws for 15-19 sexes combined
envDraws_SAMP_15to19 <- fn_rand_draw_env(envDraws_15to19, v_sample$env)
rm(envDraws_15to19)

# Calculate CSMF draws for aggregate age groups
#test <- fn_calc_agg_ages(5, 14, codAll, csmfSqzDraws_05to09[[1]], csmfSqzDraws_10to14[[1]], UNCERTAINTY = TRUE)
#csmfSqzDraws_AGG_05to14 <- mapply(function(a,b,c,d,e){ fn_calc_agg_ages(5, 14, codAll, csmfSqzDraws_05to09, csmfSqzDraws_10to14, SIMPLIFY = FALSE)})
#csmfSqzDraws_AGG_05to19 <- mapply(function(a,b,c,d,e){ fn_calc_agg_ages(5, 19, codAll, csmfSqzDraws_05to09, csmfSqzDraws_10to14, csmfSqzDraws_15to19f, csmfSqzDraws_15to19m,  envDraws_SAMP_15to19, SIMPLIFY = FALSE)})
#csmfSqzDraws_AGG_10to19 <- mapply(function(a,b,c,d,e){ fn_calc_agg_ages(10, 19, codAll, csmfSqzDraws_10to14, csmfSqzDraws_15to19f, csmfSqzDraws_15to19m, envDraws_SAMP_15to19, SIMPLIFY = FALSE)})
csmfSqzDraws_AGG_05to14 <- fn_call_agg_ages(AGELB=5, AGEUB= 14, CODALL= codAll, CSMF_5TO9 = csmfSqzDraws_05to09, CSMF_10TO14 = csmfSqzDraws_10to14, UNCERTAINTY = TRUE)
csmfSqzDraws_AGG_05to19 <- fn_call_agg_ages(AGELB=5, AGEUB= 19, CODALL= codAll, CSMF_5TO9 = csmfSqzDraws_05to09, CSMF_10TO14 = csmfSqzDraws_10to14,
                                            CSMF_15TO19F = csmfSqzDraws_15to19f, CSMF_15TO19M = csmfSqzDraws_15to19m,  ENV = envDraws_SAMP_15to19,
                                            UNCERTAINTY = TRUE)
csmfSqzDraws_AGG_10to19 <- fn_call_agg_ages(AGELB=10, AGEUB= 19, CODALL= codAll, CSMF_10TO14 = csmfSqzDraws_10to14,
                                            CSMF_15TO19F = csmfSqzDraws_15to19f, CSMF_15TO19M = csmfSqzDraws_15to19m,  ENV = envDraws_SAMP_15to19,
                                            UNCERTAINTY = TRUE)

# Calculate uncertainty intervals
ui_05to14 <- fn_calc_ui(csmfSqzDraws_AGG_05to14, UI = 0.95, CODALL = codAll)
ui_05to19 <- fn_calc_ui(csmfSqzDraws_AGG_05to19, UI = 0.95, CODALL = codAll)
ui_10to19 <- fn_calc_ui(csmfSqzDraws_AGG_10to19, UI = 0.95, CODALL = codAll)

# Combine point estimates with uncertainty intervals
pointInt_05to14 <- fn_combine_ui_point(ui_05to14, csmfSqz_AGG_05to14, codAll)
pointInt_05to19 <- fn_combine_ui_point(ui_05to19, csmfSqz_AGG_05to19, codAll)
pointInt_10to19 <- fn_combine_ui_point(ui_10to19, smfSqz_AGG_10to19, codAll)

# Round point estimates with uncertainty intervals
pointInt_FRMT_05to14 <- fn_round_pointint(pointInt_05to14, codAll)
pointInt_FRMT_05to19 <- fn_round_pointint(pointInt_05to19, codAll)
pointInt_FRMT_10to19 <- fn_round_pointint(pointInt_10to19, codAll)

# Audit: check if point estimates fall in uncertainty bounds
pointInt_AUD_05to14 <- fn_check_ui(pointInt_FRMT_05to14, codAll)
pointInt_AUD_05to19 <- fn_check_ui(pointInt_FRMT_05to19, codAll)
pointInt_AUD_10to19 <- fn_check_ui(pointInt_FRMT_10to19, codAll)
