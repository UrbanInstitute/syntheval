#' 
#' Summarize the confidence assigned to predicted classes
#' 
#' @param target An `attribute_target` object with a classification mode.
#' 
#' @returns A tibble with columns:
#' * `modeling_data`: the data the attack model was fit on, a factor with 
#'   levels `"synthetic"` and, if available, `"holdout"`.
#' * `prediction_data`: the data the confidence is evaluated on, a factor with 
#'   levels `"training"` and `"testing"` (the internal split of the modeling 
#'   data) and `"confidential"` (the attack result).
#' * `n`: the number of records in `prediction_data`.
#' * `confidence`: the average, across records, of the probability assigned 
#'   to each record's predicted class.
#' 
#' @export
#' 
target_confidence <- function(target) {
  
  stopifnot(is_attribute_target(target))
  
  if (target$mode != "classification") {
    
    stop("Error: target_confidence() requires a classification target.")
    
  }
  
  result <- .target_metric_grid(
    target = target,
    metric_name = "confidence",
    metric_fn = function(predictions) {
      
      levels_truth <- levels(predictions[[target$target_var]])
      
      # one column of predicted probability per class level, one row per record
      prob_df <- predictions[paste0(".pred_", levels_truth)]
      
      # pmap_dbl() walks the class-probability columns row by row, passing each
      # record's per-class probabilities as separate arguments to pmax() - i.e.
      # each record's probability for whichever class was actually predicted
      row_max_prob <- purrr::pmap_dbl(prob_df, pmax)
      
      mean(row_max_prob)
      
    }
  )
  
  return(result)
  
}
