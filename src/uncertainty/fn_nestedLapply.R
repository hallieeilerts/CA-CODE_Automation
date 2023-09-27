fn_nestedLapply <- function(X, FUN){ 
  
  #' @title Nested lapply()
  # 
  #' @description Performs lapply on list of lists
  #
  #' @param X List of lists.
  #' @param FUN Function to be performed on nested list elements.
  #' @return List of lists with second-level elements altered by FUN.
  
  lapply(X, function(sublist) { lapply(sublist, FUN) }) 
}
