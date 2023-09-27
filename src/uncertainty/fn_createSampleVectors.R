fn_createSampleVectors <- function(L_CSMFDRAWS_HMM, L_CSMFDRAWS_LMM, L_ENVDRAWS){
  
  #' @title Create randomly sampled vectors for igme draws and predicted fractions
  # 
  #' @description Randomly samples draws from envelopes and predicted LMM and HMM fractions.
  #' Saves sample vectors in a list.
  #
  #' @param L_CSMFDRAWS_HMM List of length number of draws.
  #' Each list element is a data frame with CSMFs for all years being predicted for HMM countries.
  #' @param L_CSMFDRAWS_LMM List of length number of draws.
  #' Each list element is a data frame with CSMFs for all years being predicted for LMM countries.
  #' @param L_ENVDRAWS List of length three, corresponding to crisis-free deaths, crisis-included deaths, crisis-included rates.
  #' Each first-level list element is a list of length number of draws.
  #' Within first-level list element, each second-level list element is a data frame 
  #' with a draw with columns c('ISO3', 'Year', 'Sex', 'Deaths1', 'Deaths2', 'Rate').
  #' @return List of length three, corresponding to envelope, HMM, LMM.
  #' Each list element is a vector (all of equal length) with a list of integers.
  #' Integers are randomly sampled draws from envelope, HMM, LMM.
  
  if(length(L_CSMFDRAWS_HMM) >= length(L_CSMFDRAWS_LMM)){
    # Sample from IGME draws, number of samples = number of draws for predicted fractions
    #v_sample_env <- sort(sample(x = dim(L_ENVDRAWS$deaths)[3], size = length(L_CSMFDRAWS_HMM))) 
    v_sample_env <- sort(sample(x = length(L_ENVDRAWS$deaths1), size = length(L_CSMFDRAWS_HMM))) 
    
    # Sets of HMM fractions
    v_sample_HMM <- 1:length(L_CSMFDRAWS_HMM)
    
    # Sets of LMM fractions
    # With extra samples if there are fewer LMM draws than HMM
    v_sample_LMM <- c(1:length(L_CSMFDRAWS_LMM),
                      sort(sample(x = 1:length(L_CSMFDRAWS_LMM),
                                  size = length(L_CSMFDRAWS_HMM) - length(L_CSMFDRAWS_LMM), replace = T)))
  }else{
    
    # Sample from IGME draws, number of samples = number of draws for predicted fractions
    #v_sample_env <- sort(sample(x = dim(L_ENVDRAWS$deaths)[3], size = length(L_CSMFDRAWS_LMM)))
    v_sample_env <- sort(sample(x = length(L_ENVDRAWS$deaths1), size = length(L_CSMFDRAWS_LMM))) 
    
    # Sets of HMM fractions
    # With extra samples if there are fewer HMM draws than LMM
    v_sample_HMM <- c(1:length(L_CSMFDRAWS_HMM),
                      sort(sample(x = 1:length(L_CSMFDRAWS_HMM),
                                  size = length(L_CSMFDRAWS_LMM) - length(L_CSMFDRAWS_HMM), replace = T)))
    
    # Sets of LMM fractions
    v_sample_LMM <- 1:length(L_CSMFDRAWS_LMM)
  }
  
  # Combine all sample vectors into list
  v_sample <- list(v_sample_env, v_sample_HMM, v_sample_LMM)
  names(v_sample) <- c("env", "HMM", "LMM")
  
  return(v_sample)
}
