#' 
#' Measure the quality of predicted class probabilities
#' 
#' @param target An `attribute_target` object with a classification mode.
#' @param on Either `"confidential"` (default, the attack result), `"train"`, 
#' or `"test"` (model-quality diagnostics on the fitting data).
#' 
#' @returns A tibble with columns `source` (`"synthetic"` or `"holdout"`), 
#' `on`, and `log_loss`.
#' 
#' @export
#' 
target_log_loss <- function(target, on = "confidential") {
  
  stopifnot(is_attribute_target(target))
  
  if (target$mode != "classification") {
    
    stop("Error: target_log_loss() requires a classification target.")
    
  }
  
  on <- match.arg(on, c("confidential", "train", "test"))
  
  result <- .target_metric_table(
    target = target,
    on = on,
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
