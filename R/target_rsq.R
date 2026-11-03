#' 
#' Measure the proportion of variation in the confidential target explained by the predictions
#' 
#' @param target An `attribute_target` object with a regression mode.
#' 
#' @returns A tibble with columns:
#' * `modeling_data`: the data the attack model was fit on, a factor with 
#'   levels `"synthetic"` and, if available, `"holdout"`.
#' * `prediction_data`: the data the R-squared is evaluated on, a factor with 
#'   levels `"training"` and `"testing"` (the internal split of the modeling 
#'   data) and `"confidential"` (the attack result).
#' * `n`: the number of records in `prediction_data`.
#' * `rsq`: the squared correlation between predicted and observed values.
#' 
#' @export
#' 
target_rsq <- function(target) {
  
  stopifnot(is_attribute_target(target))
  
  if (target$mode != "regression") {
    
    stop("Error: target_rsq() requires a regression target.")
    
  }
  
  result <- .target_metric_grid(
    target = target,
    metric_name = "rsq",
    metric_fn = function(predictions) {
      
      yardstick::rsq_vec(
        truth = predictions[[target$target_var]],
        estimate = predictions$.pred
      )
      
    }
  )
  
  return(result)
  
}
