#' 
#' Measure the mean absolute error between predicted and observed confidential values
#' 
#' @param target An `attribute_target` object with a regression mode.
#' 
#' @returns A tibble with columns:
#' * `modeling_data`: the data the attack model was fit on, a factor with 
#'   levels `"synthetic"` and, if available, `"holdout"`.
#' * `prediction_data`: the data the MAE is evaluated on, a factor with levels 
#'   `"training"` and `"testing"` (the internal split of the modeling data) and 
#'   `"confidential"` (the attack result).
#' * `n`: the number of records in `prediction_data`.
#' * `mae`: the mean absolute error.
#' 
#' @export
#' 
target_mae <- function(target) {
  
  stopifnot(is_attribute_target(target))
  
  if (target$mode != "regression") {
    
    stop("Error: target_mae() requires a regression target.")
    
  }
  
  result <- .target_metric_grid(
    target = target,
    metric_name = "mae",
    metric_fn = function(predictions) {
      
      yardstick::mae_vec(
        truth = predictions[[target$target_var]],
        estimate = predictions$.pred
      )
      
    }
  )
  
  return(result)
  
}
