#' 
#' Measure classification accuracy while accounting for class imbalance
#' 
#' @param target An `attribute_target` object with a classification mode.
#' @param on Either `"confidential"` (default, the attack result), `"train"`, 
#' or `"test"` (model-quality diagnostics on the fitting data).
#' 
#' @returns A tibble with columns `source` (`"synthetic"` or `"holdout"`), 
#' `on`, and `balanced_accuracy`.
#' 
#' @export
#' 
target_balanced_accuracy <- function(target, on = "confidential") {
  
  stopifnot(is_attribute_target(target))
  
  if (target$mode != "classification") {
    
    stop("Error: target_balanced_accuracy() requires a classification target.")
    
  }
  
  on <- match.arg(on, c("confidential", "train", "test"))
  
  result <- .target_metric_table(
    target = target,
    on = on,
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
