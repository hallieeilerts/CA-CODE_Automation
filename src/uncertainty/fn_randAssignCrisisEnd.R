fn_randAssignCrisisEnd <- function(CSMFDRAW){
  
  #' @title Randomly assign endemic crisis deaths for current draw
  # 
  #' @description If there are any endemic crisis deaths, sample from multinomial distribution.
  #' 
  #' @param CSMFDRAW Data frame that is one draw of predicted CSMFs, single cause data, envelopes, and minimum fractions.
  #' @return Data frame with randomly sampled endemic crisis deaths.
  
  dat <- CSMFDRAW
  
  # Add crisis-free deaths with endemic CollectVio and NatDis
  v_deaths <- dat$Deaths1 + dat$CollectVio + dat$NatDis
  
  # Calculate fraction of endemic collective violence (Pro-rata squeeze)
  dat$CollectVio <- dat$CollectVio/v_deaths
  
  # Calculate fraction of endemic natural disaster (Pro-rata squeeze)
  dat$NatDis <- dat$NatDis/v_deaths
  
  # Values to be squeezed
  v_idSqz <- which(dat$CollectVio != 0 | dat$NatDis != 0)
  
  # Sample random values
  if (length(v_idSqz) > 0) {
    # Create data.frame with csmfs for inverse of endemic crisis, CollectVio, NatDis, and crisis-free deaths
    datAux <- cbind(1 - dat$CollectVio[v_idSqz] - dat$NatDis[v_idSqz],
                    dat$CollectVio[v_idSqz], dat$NatDis[v_idSqz], 
                    dat$Deaths1[v_idSqz])
    # Randomly sample from crisis-free deaths with probability equivalent to the three CSMFs
    datAux <- t(apply(datAux, 1,
                      function(x) {
                        rmultinom(n = 1, size = round(x[4]), prob = x[1:3])
                      }))
    # Update the Collective Violence and Natural Disasters fractions for the current draw
    dat$CollectVio[v_idSqz] <- datAux[, 2]/dat$Deaths1[v_idSqz]
    dat$NatDis[v_idSqz] <- datAux[, 3]/dat$Deaths1[v_idSqz]
  }
  
  return(dat)
}
