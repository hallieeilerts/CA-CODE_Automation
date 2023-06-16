
##################################################
####
####   Upload all-cause and crisis-free draws (rates and deaths)
####
##################################################

fn_format_igme_draws <- function(dea, rate, key_regclass) {
  
  ## dea         Path to file with death draws
  ## rate        Path to file with rates draws
  
  # List of countries
  load('./data/igme/draws/info.rda')
  
  # Male draws
  # deathsMen <- deathsAllMen <- ratesAllMen <- NULL
  
  # This function selects columns which pertain to Years. 
  # The code "- (length(Years)-1):0" used to be "- 21:0"
  # !!! Need to ensure that latest draw is same as highest value in Years.
  warning("Ensure that latest Draw is the same as latest year being predicted.")
  
  #--------------------#
  # CRISIS-FREE DEATHS #
  #--------------------#
  
  # Draws location
  load(paste0('./data/igme/draws/crisis-free/', dea))
  
  # Select years
  if (ageLow %in% c(5, 15)) {
    deaths <- death0.ctj[, dim(death0.ctj)[2] - (length(Years)-1):0, ] 
    deaths[1:5,1:22,1]
    rm(death0.ctj)
  }
  if (ageLow == 10) {
    deaths <- death1to4.ctj[, dim(death1to4.ctj)[2] - (length(Years)-1):0, ]
    rm(death1to4.ctj)
  }
  
  
  #------------------#
  # ALL CAUSE DEATHS #
  #------------------#
  
  # Draws location
  load(paste0('./data/igme/draws/crisis-included/', dea))
  
  # Select years
  if (ageLow %in% c(5, 15)) {
    deathsAll <- death0.ctj[, dim(death0.ctj)[2] - (length(Years)-1):0, ]
    rm(death0.ctj)
  }
  if (ageLow == 10) {
    deathsAll <- death1to4.ctj[, dim(death1to4.ctj)[2] - (length(Years)-1):0, ]
    rm(death1to4.ctj)
  }
  
  #-----------------#
  # ALL CAUSE RATES #
  #-----------------#
  
  # Draws location
  load(paste0('./data/igme/draws/crisis-included/', rate))
  
  # Select years
  if (ageLow %in% c(5, 15)) {
    ratesAll <- imr.ctj[, dim(imr.ctj)[2] - (length(Years)-1):0, ]
    rm(imr.ctj)
  }
  if (ageLow == 10) {
    ratesAll <- cmr.ctj[, dim(cmr.ctj)[2] - (length(Years)-1):0, ]
    rm(cmr.ctj)
  }
  
  # Country labels
  dimnames(deaths) <- dimnames(deathsAll) <- 
    dimnames(ratesAll) <- list(info$iso.c, Years, NULL)
  
  # Select countries
  deaths <- deaths[which(info$iso.c %in% key_regclass$ISO3), , ]
  deathsAll <- deathsAll[which(info$iso.c %in% key_regclass$ISO3), , ]
  ratesAll <- ratesAll[which(info$iso.c %in% key_regclass$ISO3), , ]
  
  # Exclude draws with inconsistencies
  dif <- deathsAll - deaths
  idExclude <- c()
  for (i in 1:dim(dif)[3]) {
    if (any(dif[,,i] < 0, na.rm = T)) idExclude <- c(idExclude, i)
  }
  if (length(idExclude) > 0) {
    deaths <- deaths[, , -idExclude]
    deathsAll <- deathsAll[, , -idExclude]
    ratesAll <- ratesAll[, , -idExclude]
  }

  # Output
  return(list(deaths = deaths, deathsAll = deathsAll, ratesAll = ratesAll))
  
}


##################################################
####
####   Format predicted fractions for draws
####
##################################################

fn_rearrange_draws <- function(L_DRAWS){

  # Transform from a list of lists (first level = year, second level = draws)
  # Into a list where each element is a year containing all draws for that year
  L_DRAWS <- lapply(L_DRAWS, function(x){ names(x) <- 1:length(x)
                                                  x <- ldply(x, .id = "draw")
                                                  return(x)})
  df_draws <- ldply(L_DRAWS)
  
  # Split into list by "draw" column
  l_draws <- split(df_draws, df_draws$draw)
  l_draws <- lapply(l_draws, function(x){ x <- x[order(x$ISO3, x$Year),]})
  
  # Tidy up
  l_draws <- lapply(l_draws, function(x){x$draw <- NULL ; return(x)})
  l_draws <- lapply(l_draws, function(x){ x[, c(idVars, sort(names(x)[which(!names(x) %in% idVars)]))] ; return(x)})
  l_draws <- lapply(l_draws, function(x){ rownames(x) <- NULL ; return(x)})
  
  return(l_draws)
}


######## Combining HMM and LMM

# L_DRAWS_HMM <- l_draws_HMM
# L_DRAWS_LMM <- l_draws_LMM
# 
# # Transform from a list of lists (first level = year, second level = draws)
# # Into a list where each element is a year containing all draws for that year
# L_DRAWS_HMM <- lapply(L_DRAWS_HMM, function(x){ names(x) <- 1:length(x)
#                                                 x <- ldply(x, .id = "draw")
#                                                 return(x)})
# df_draws_HMM <- ldply(L_DRAWS_HMM)
# L_DRAWS_LMM <- lapply(L_DRAWS_LMM, function(x){ names(x) <- 1:length(x)
#                                                 x <- ldply(x, .id = "draw")
#                                                 return(x)})
# df_draws_LMM <- ldply(L_DRAWS_LMM)
# 
# # Combine LMM and HMM
# df_draws <- rbind(df_draws_LMM, df_draws_HMM)
# # Split into list by "draw" column
# l_draws <- split(df_draws, df_draws$draw)
# l_draws <- lapply(l_draws, function(x){ x <- x[order(x$ISO3, x$Year),]})
# 
# # Tidy up
# l_draws <- lapply(l_draws, function(x){x$draw <- NULL ; return(x)})
# l_draws <- lapply(l_draws, function(x){ x[, c(idVars, sort(names(x)[which(!names(x) %in% idVars)]))] ; return(x)})
# l_draws <- lapply(l_draws, function(x){ rownames(x) <- NULL ; return(x)})


### old ###

# First level of list is year. there are 22 elements.
# Within each year, there are ~1000 draws
# Need to combine all the first draws from each element, etc.
# df_test <- lapply(l_test, function(x){ x <- ldply(x); return(x)})
# 
# fn_format_predicted_draws <- function(L_DRAWS_HMM, L_DRAWS_LMM){
#   
#   
#   mapply(function(X,Y) {
#     sapply(1:10, function(row) cor(X[row,], Y[row,]))
#   }, X=L_DRAWS_HMM, L_DRAWS_LMM)
#   
#   L_DRAWS <- mapply(cbind, L_DRAWS_HMM, L_DRAWS_HMM, SIMPLIFY=F)
#   
#   
#   
#   
#   
#   datHMM <- do.call(rbind, L_DAT_HMM)
#   datLMM <- do.call(rbind, L_DAT_LMM)
#   dat <- rbind(datHMM, datLMM)
#   
#   # Rearrange columns
#   dat <- dat[, c(idVars, sort(names(dat)[which(!names(dat) %in% idVars)]))] 
#   
#   # Tidy up
#   dat <- dat[order(dat$ISO3, dat$Year),]
#   
#   return(dat)
#   
# }



##################################################
####
####   Nested lapply
####
##################################################


fn_nested_lapply <- function(data, fun) { lapply(data, function(sublist) { lapply(sublist, fun) })  }


##################################################
####
####  Create randomly sampled vectors for igme draws and predicted fractions
####
##################################################

L_DRAWS_HMM <- l_draws_Rearranged_HMM
L_DRAWS_LMM <- l_draws_Rearranged_LMM
L_DRAWS_IGME <-l_draws_igme

fn_create_rand_vectors <- function(L_DRAWS_HMM, L_DRAWS_LMM, L_DRAWS_IGME){
  
  if(length(L_DRAWS_HMM) >= length(L_DRAWS_LMM)){
    # Sample from IGME draws, number of samples = number of draws for predicted fractions
    v_rand_igme <- sort(sample(x = dim(L_DRAWS_IGME$deaths)[3], size = length(L_DRAWS_HMM))) 
    
    # Sets of HMM fractions
    v_rand_HMM <- 1:length(L_DRAWS_HMM)
    
    # Sets of LMM fractions
    # With extra samples if there are fewer LMM draws than HMM
    v_rand_LMM <- c(1:length(L_DRAWS_LMM),
                    sort(sample(x = 1:length(L_DRAWS_LMM),
                                size = length(L_DRAWS_HMM) - length(L_DRAWS_LMM), replace = T)))
  }else{
    
    # Sample from IGME draws, number of samples = number of draws for predicted fractions
    v_rand_igme <- sort(sample(x = dim(L_DRAWS_IGME$deaths)[3], size = length(L_DRAWS_LMM))) 
    
    # Sets of HMM fractions
    # With extra samples if there are fewer HMM draws than LMM
    v_rand_HMM <- c(1:length(L_DRAWS_HMM),
                    sort(sample(x = 1:length(L_DRAWS_HMM),
                                size = length(L_DRAWS_LMM) - length(L_DRAWS_HMM), replace = T)))
    
    # Sets of LMM fractions
    v_rand_LMM <- 1:length(L_DRAWS_LMM)
  }
  
  # Combine all sample vectors into list
  v_rand <- list(v_rand_igme, v_rand_HMM, v_rand_LMM)
  names(v_rand) <- c("IGME", "HMM", "LMM")
  
  return(v_rand)
}


##################################################
####
####  Random draw from igme
####
##################################################

L_DRAWS_IGME <-l_draws_igme
V_RAND <- v_rand$IGME

fn_rand_draw_igme <- function(L_DRAWS_IGME, V_RAND)

# Crisis-free IGME deaths (random draw)
l_deaths1 <- lapply(V_RAND, function(x) as.data.frame(L_DRAWS_IGME$deaths[, , x]))
l_deaths1 <- lapply(l_deaths1, function(x){ x$ISO3 <- rownames(x) ; return(x)})
l_deaths1 <- lapply(l_deaths1, function(x){ x <- as.data.frame(pivot_longer(x, cols = 1:(ncol(x)-1),
                                                   names_to = 'Year', values_to = 'Deaths1')) ; return(x)})
l_deaths1 <- lapply(l_deaths1, function(x){ x$Sex <- sexLabel ; return(x)})

# All-cause IGME deaths (random draw)
l_deaths2 <- lapply(V_RAND, function(x) as.data.frame(L_DRAWS_IGME$deathsAll[, , x]))
l_deaths2 <- lapply(l_deaths2, function(x){ x$ISO3 <- rownames(x) ; return(x)})
l_deaths2 <- lapply(l_deaths2, function(x){ x <- as.data.frame(pivot_longer(x, cols = 1:(ncol(x)-1),
                                                                            names_to = 'Year', values_to = 'Deaths2')) ; return(x)})
l_deaths2 <- lapply(l_deaths2, function(x){ x$Sex <- sexLabel ; return(x)})

# All-cause IGME rates (random draw)
l_rates2 <- lapply(V_RAND, function(x) as.data.frame(L_DRAWS_IGME$ratesAll[, , x]))
l_rates2 <- lapply(l_rates2, function(x){ x$ISO3 <- rownames(x) ; return(x)})
l_rates2 <- lapply(l_rates2, function(x){ x <- as.data.frame(pivot_longer(x, cols = 1:(ncol(x)-1),
                                                                            names_to = 'Year', values_to = 'Rate')) ; return(x)})
l_rates2 <- lapply(l_rates2, function(x){ x$Sex <- sexLabel ; return(x)})

# Merge estimates
#mapply(merge(), first, second, SIMPLIFY=FALSE)
#drawIGME <- merge(deaths1, deaths2, by = idVars)
#drawIGME <- merge(drawIGME, rates2, by = idVars)
#head(Reduce(function(x, y) merge(x, y, all=TRUE), list(l_deaths1[1], l_deaths2[1], l_rates2[2])))

fn_merge_3list <-function(list1, list2, list3){ Reduce(function(x, y) merge(x, y, by = idVars, all=TRUE), list(list1, list2, list3))}
l_randdraw_igme <- mapply(fn_merge_3list, l_deaths1, l_deaths2, l_rates2, SIMPLIFY=FALSE)

#--------------------------------#
# UN IGME ENVELOPES: RANDOM DRAW #
#--------------------------------#

i <- 1

# Crisis-free IGME deaths (random draw)
deaths1 <- as.data.frame(L_DRAWS_IGME$deaths[, , randDraws[i]])
deaths1$ISO3 <- rownames(deaths1)
deaths1 <- as.data.frame(pivot_longer(deaths1, cols = 1:(ncol(deaths1)-1),
                                      names_to = 'Year', values_to = 'Deaths1'))
deaths1$Sex <- sexLabel

# All-cause IGME deaths (random draw)
deaths2 <- as.data.frame(L_DRAWS_IGME$deathsAll[, , randDraws[i]])
deaths2$ISO3 <- rownames(deaths2)
deaths2 <- as.data.frame(pivot_longer(deaths2, cols = 1:(ncol(deaths2)-1),
                                      names_to = 'Year', values_to = 'Deaths2'))
deaths2$Sex <-  sexLabel

# All-cause IGME rates (random draw)
rates2 <- as.data.frame(L_DRAWS_IGME$ratesAll[, , randDraws[i]])
rates2$ISO3 <- rownames(rates2)
rates2 <- as.data.frame(pivot_longer(rates2, cols = 1:(ncol(rates2)-1),
                                     names_to = 'Year', values_to = 'Rate'))
rates2$Sex <- sexLabel

# Merge estimates
drawIGME <- merge(deaths1, deaths2, by = idVars)
drawIGME <- merge(drawIGME, rates2, by = idVars)

#------------------------#
# 2023.06.09 PATCH

# Avoid NA on draws
drawIGME$Deaths2[which(is.na(drawIGME$Deaths2))] <- 0
drawIGME$Deaths1[which(is.na(drawIGME$Deaths1))] <- 0
drawIGME$Rate[which(is.na(drawIGME$Rate))] <- 0

# END PATCH
#------------------------#
