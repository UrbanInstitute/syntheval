#' 
#' Measure the ability of predicted probabilities to distinguish between classes
#' 
#' @param target An `attribute_target` object with a classification mode.
#' 
#' @returns A tibble with columns:
#' * `modeling_data`: the data the attack model was fit on, a factor with 
#'   levels `"synthetic"` and, if available, `"holdout"`.
#' * `prediction_data`: the data the AUC is evaluated on, a factor with levels 
#'   `"training"` and `"testing"` (the internal split of the modeling data) and 
#'   `"confidential"` (the attack result).
#' * `n`: the number of records in `prediction_data`.
#' * `auc`: the area under the ROC curve.
#' 
#' @export
#' 
target_auc <- function(target) {
  
  stopifnot(is_attribute_target(target))
  
  if (target$mode != "classification") {
    
    stop("Error: target_auc() requires a classification target.")
    
  }
  
  result <- .target_metric_grid(
    target = target,
    metric_name = "auc",
    metric_fn = function(predictions) {
      
      yardstick::roc_auc_vec(
        truth = predictions[[target$target_var]],
        estimate = .class_prob_estimate(predictions, target$target_var)
      )
      
    }
  )
  
  return(result)
  
}
