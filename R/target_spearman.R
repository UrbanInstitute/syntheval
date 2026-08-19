#' 
#' Measure rank-order agreement between predicted and observed confidential values
#' 
#' @param target An `attribute_target` object with a regression mode.
#' @param on Either `"confidential"` (default, the attack result), `"train"`, 
#' or `"test"` (model-quality diagnostics on the fitting data).
#' 
#' @returns A tibble with columns `source` (`"synthetic"` or `"holdout"`), 
#' `on`, and `spearman`.
#' 
#' @export
#' 
target_spearman <- function(target, on = "confidential") {
  
  stopifnot(is_attribute_target(target))
  
  if (target$mode != "regression") {
    
    stop("Error: target_spearman() requires a regression target.")
    
  }
  
  on <- match.arg(on, c("confidential", "train", "test"))
  
  result <- .target_metric_table(
    target = target,
    on = on,
    metric_name = "spearman",
    metric_fn = function(predictions) {
      
      stats::cor(
        predictions[[target$target_var]],
        predictions$.pred,
        method = "spearman"
      )
      
    }
  )
  
  return(result)
  
}
