#' 
#' Measure the quality of predicted class probabilities
#' 
#' @param target An `attribute_target` object with a classification mode.
#' 
#' @returns A tibble with columns:
#' * `modeling_data`: the data the attack model was fit on, a factor with 
#'   levels `"synthetic"` and, if available, `"holdout"`.
#' * `prediction_data`: the data the log loss is evaluated on, a factor with 
#'   levels `"training"` and `"testing"` (the internal split of the modeling 
#'   data) and `"confidential"` (the attack result).
#' * `n`: the number of records in `prediction_data`.
#' * `log_loss`: the mean log loss of the predicted class probabilities.
#' 
#' @export
#' 
target_log_loss <- function(target) {
  
  stopifnot(is_attribute_target(target))
  
  if (target$mode != "classification") {
    
    stop("Error: target_log_loss() requires a classification target.")
    
  }
  
  result <- .target_metric_grid(
    target = target,
    metric_name = "log_loss",
    metric_fn = function(predictions) {
      
      yardstick::mn_log_loss_vec(
        truth = predictions[[target$target_var]],
        estimate = .class_prob_estimate(predictions, target$target_var)
      )
      
    }
  )
  
  return(result)
  
}
