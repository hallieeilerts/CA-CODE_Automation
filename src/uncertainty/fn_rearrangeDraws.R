fn_rearrangeDraws <- function(L_CSMFDRAWS){
  
  #' @title Rearrange draws of predicted fractions
  # 
  #' @description Transforms nested list (first level = year, second level = draws) into a list 
  #' where each element is a draw which includes all predicted CSMFs for that year.
  #
  #' @param L_CSMFDRAWS List of length number of years being predicted.
  #' Each first-level list element is a list of length number of draws.
  #' Within first-level list element, each second-level list element is a data frame with predicted CSMFs for one year.
  #' @return List of length number of draws.
  #' Each list element is a data frame with predicted CSMFs for one draw.
  
  # Add sequential name for draws
  # Transform list of all draws into a data frame with an index column for "draw"
  l_draws <- lapply(L_CSMFDRAWS, function(x){ names(x) <- 1:length(x)
  x <- ldply(x, .id = "draw")
  return(x)})
  df_draws <- ldply(l_draws)
  
  # Split into list by "draw" column
  l_draws <- split(df_draws, df_draws$draw)
  l_draws <- lapply(l_draws, function(x){ x <- x[order(x$ISO3, x$Year),]})
  
  # Tidy up
  l_draws <- lapply(l_draws, function(x){x$draw <- NULL ; return(x)})
  l_draws <- lapply(l_draws, function(x){ x[, c(idVars, sort(names(x)[which(!names(x) %in% idVars)]))] ; return(x)})
  l_draws <- lapply(l_draws, function(x){ rownames(x) <- NULL ; return(x)})
  
  return(l_draws)
}
