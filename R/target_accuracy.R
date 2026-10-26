#' 
#' Measure the proportion of confidential records whose class is correctly predicted
#' 
#' @param target An `attribute_target` object with a classification mode.
#' 
#' @returns A tibble with columns:
#' * `modeling_data`: the data the attack model was fit on, a factor with 
#'   levels `"synthetic"` and, if available, `"holdout"`.
#' * `prediction_data`: the data the accuracy is evaluated on, a factor with 
#'   levels `"training"` and `"testing"` (the internal split of the modeling 
#'   data) and `"confidential"` (the attack result).
#' * `n`: the number of records in `prediction_data`.
#' * `accuracy`: the proportion of records whose class is correctly predicted.
#' 
#' @export
#' 
target_accuracy <- function(target) {
  
  stopifnot(is_attribute_target(target))
  
  if (target$mode != "classification") {
    
    stop("Error: target_accuracy() requires a classification target.")
    
  }
  
  result <- .target_metric_grid(
    target = target,
    metric_name = "accuracy",
    metric_fn = function(predictions) {
      
      yardstick::accuracy_vec(
        truth = predictions[[target$target_var]],
        estimate = predictions$.pred_class
      )
      
    }
  )
  
  return(result)
  
}
