#' 
#' Measure the proportion of variation in the confidential target explained by the predictions
#' 
#' @param target An `attribute_target` object with a regression mode.
#' @param on Either `"confidential"` (default, the attack result), `"train"`, 
#' or `"test"` (model-quality diagnostics on the fitting data).
#' 
#' @returns A tibble with columns `source` (`"synthetic"` or `"holdout"`), 
#' `on`, and `rsq`.
#' 
#' @export
#' 
target_rsq <- function(target, on = "confidential") {
  
  stopifnot(is_attribute_target(target))
  
  if (target$mode != "regression") {
    
    stop("Error: target_rsq() requires a regression target.")
    
  }
  
  on <- match.arg(on, c("confidential", "train", "test"))
  
  result <- .target_metric_table(
    target = target,
    on = on,
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
