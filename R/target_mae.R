#' 
#' Measure the mean absolute error between predicted and observed confidential values
#' 
#' @param target An `attribute_target` object with a regression mode.
#' @param on Either `"confidential"` (default, the attack result), `"train"`, 
#' or `"test"` (model-quality diagnostics on the fitting data).
#' 
#' @returns A tibble with columns `source` (`"synthetic"` or `"holdout"`), 
#' `on`, and `mae`.
#' 
#' @export
#' 
target_mae <- function(target, on = "confidential") {
  
  stopifnot(is_attribute_target(target))
  
  if (target$mode != "regression") {
    
    stop("Error: target_mae() requires a regression target.")
    
  }
  
  on <- match.arg(on, c("confidential", "train", "test"))
  
  result <- .target_metric_table(
    target = target,
    on = on,
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
