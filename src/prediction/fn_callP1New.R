fn_callP1New <- function(PREDYEAR, MOD_FIT, DAT_PRED, UNCERTAINTY = FALSE){
  
  #' @title Call function for fn_p1New() - prediction from one model estimation
  # 
  #' @description Runs fn_p1New and adds identifying columns
  #
  #' @param PREDYEAR Integer for year being predicted.
  #' @param MOD_FIT Model fit object.
  #' @param DAT_PRED Data frame with selected covariates from prediction database for age/sex-specific model. 
  #' @param UNCERTAINTY Boolean for whether to return mean prediction or all draws from random effects.
  #' @return Data frame with selected covariates from prediction database. 
  
  # Subset to one year
  df_pred <- subset(DAT_PRED, Year == PREDYEAR)
  
  # Call prediction function
  out <- fn_p1New(MO = MOD_FIT, PRED = df_pred, DEAT = NULL, PRAN = T, MEAN = !UNCERTAINTY)
  
  if(!UNCERTAINTY){
    # Format
    df_out <- as.data.frame(out$PF)
    df_out$ISO3 <- rownames(df_out)
    df_out$Year <- PREDYEAR
    rownames(df_out) <- NULL
    df_out$Sex <- sexLabel
    
    #----------------------#
    # PATCH 2023.05.15
    # Rename predicted CODs
    # Note: Use these names in model estimation next round so there's no need to adjust.
    names(df_out)[names(df_out) == "OtherCD"] <- "OtherCMPN"
    names(df_out)[names(df_out) == "RTA"] <- "RTI"
    names(df_out)[names(df_out) == "Self_harm"] <- "SelfHarm"
    names(df_out)[names(df_out) == "Interp_violence"] <- "InterpVio"
    names(df_out)[names(df_out) == "Other_inj"] <- "OtherInj"
    # END PATCH
    #----------------------#
    # Add empty "Maternal" column for 15-19 males
    if(sexLabel == sexLabels[3]){df_out$Maternal <- 0}
    
    # Return data frame
    return(df_out)
  }else{
    
    # Format
    l_out <- lapply(seq(dim(out$PF)[3]), function(x) out$PF[ , , x])
    l_out <- lapply(l_out, function(x){ x <- as.data.frame(x) ; return(x)})
    l_out <- lapply(l_out, function(x){ x$ISO3 <- rownames(x) ; return(x)})
    l_out <- lapply(l_out, function(x){ x$Year <- PREDYEAR    ; return(x)})
    l_out <- lapply(l_out, function(x){ rownames(x) <- NULL   ; return(x)})
    l_out <- lapply(l_out, function(x){ x$Sex <- sexLabel ; return(x)})
    
    #----------------------#
    # PATCH 2023.05.15
    # Rename predicted CODs
    # Note: Use these names in model estimation next round so there's no need to adjust.
    l_out <- lapply(l_out, function(x){ names(x)[names(x) == "OtherCD"] <- "OtherCMPN"
    names(x)[names(x) == "RTA"] <- "RTI"
    names(x)[names(x) == "Self_harm"] <- "SelfHarm"
    names(x)[names(x) == "Interp_violence"] <- "InterpVio"
    names(x)[names(x) == "Other_inj"] <- "OtherInj" ; return(x)})
    # END PATCH
    #----------------------#
    # Add empty "Maternal" column for 15-19 males
    if(sexLabel == sexLabels[3]){l_out <- lapply(l_out, function(x){ x$Maternal <- 0 ; return(x)})}
    
    # return list
    return(l_out)
    
  }
  
}
