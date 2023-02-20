#' calculate null pmse
#'
#' @param data_orig A data frame with the original data
#' @param times The number of permutations
#' @param formula A formula for the CART model
#' @param cp A hyperparameter for CART
#'
#' @importFrom stats predict
#'
#' @return joint distributional utility metric under the null of two data sets drawn from the same distribution (num)
#'
#' @export
#'
null_pmse <- function(data_orig, times, formula = id ~ ., cp){
  
  pmse_null <- vector(mode = "numeric", length = times)
  
  for (a in seq_along(pmse_null)) {
    
    # bootstrap 2 times the rows
    # combine and add indicators
    comb_data <- data_orig %>%
      dplyr::slice_sample(n = nrow(data_orig) * 2, replace = TRUE) %>%
      dplyr::mutate(id = rep(c("0", "1"), each = nrow(data_orig)))
    
    ## fit model and predict prop scores
    prop_cart <- rpart::rpart(formula = formula, data = comb_data, cp = cp, method = "class")
    pred_prob <- predict(prop_cart)[, 2]
    
    ## calculate pmse
    pmse_null[a] <- mean((pred_prob - 0.5) ^ 2)
    
  }
  
  mean_null_pmse <- mean(pmse_null)
  
  return(mean_null_pmse)
  
}