#' 
#' Measure rank-order agreement between predicted and observed confidential values
#' 
#' @param target An `attribute_target` object with a regression mode.
#' 
#' @returns A tibble with columns:
#' * `modeling_data`: the data the attack model was fit on, a factor with 
#'   levels `"synthetic"` and, if available, `"holdout"`.
#' * `prediction_data`: the data the Spearman correlation is evaluated on, a 
#'   factor with levels `"training"` and `"testing"` (the internal split of the 
#'   modeling data) and `"confidential"` (the attack result).
#' * `n`: the number of records in `prediction_data`.
#' * `spearman`: the Spearman rank correlation between predicted and observed 
#'   values.
#' 
#' @export
#' 
target_spearman <- function(target) {
  
  stopifnot(is_attribute_target(target))
  
  if (target$mode != "regression") {
    
    stop("Error: target_spearman() requires a regression target.")
    
  }
  
  result <- .target_metric_grid(
    target = target,
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
