#' Calculate the pMSE using a decision tree
#'
#' @param postsynth An object of class `postsynth`.
#' @param data The original data set.
#' @param formula A formula for the model for calculating propensity scores.
#' @param times The number of bootstrap samples to use when simulating the 
#' null_pmse.
#' @param cp The complexity parameter for the decision tree model.
#' @param null_pmse An optional null_pmse to save computation time from 
#' simulating the null_pmse.
#'
#' @return A list with the pMSE, null pMSE, and pMSE ratio.
#' 
#' @export
#'
pmse_ratio <- function(postsynth, 
                       data, 
                       formula = id ~ .,
                       times = 100, 
                       cp = 0.01, 
                       null_pmse = NULL) {
  
  # if the null is not provided, then calculate the null
  if (is.null(null_pmse)) {
    
    null_pmse <- null_pmse(
      data_orig = data, 
      times = times, 
      formula = formula, 
      cp = cp
    )
    
  }
  
  # calculate the pmse
  pmse <- pmse(
    data_orig = data, 
    data_syn = postsynth$synthetic_data, 
    formula = formula, 
    cp = cp
  )
  
  # calculate the ratio
  pmse_ratio <- pmse$pmse / null_pmse
  
  return(
    list(
      pmse_ratio = pmse_ratio,
      pmse = pmse$pmse,
      null_pmse = null_pmse,
      variable_importance = pmse$variable_importance
    )
  )
  
}