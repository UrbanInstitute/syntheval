#' 
#' Measure classification accuracy while accounting for class imbalance
#' 
#' @param target An `attribute_target` object with a classification mode.
#' 
#' @returns A tibble with columns:
#' * `modeling_data`: the data the attack model was fit on, a factor with 
#'   levels `"synthetic"` and, if available, `"holdout"`.
#' * `prediction_data`: the data the balanced accuracy is evaluated on, a 
#'   factor with levels `"training"` and `"testing"` (the internal split of the 
#'   modeling data) and `"confidential"` (the attack result).
#' * `n`: the number of records in `prediction_data`.
#' * `balanced_accuracy`: the average of sensitivity and specificity.
#' 
#' @export
#' 
target_balanced_accuracy <- function(target) {
  
  stopifnot(is_attribute_target(target))
  
  if (target$mode != "classification") {
    
    stop("Error: target_balanced_accuracy() requires a classification target.")
    
  }
  
  result <- .target_metric_grid(
    target = target,
    metric_name = "balanced_accuracy",
    metric_fn = function(predictions) {
      
      yardstick::bal_accuracy_vec(
        truth = predictions[[target$target_var]],
        estimate = predictions$.pred_class
      )
      
    }
  )
  
  return(result)
  
}
