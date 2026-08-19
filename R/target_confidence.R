#' 
#' Summarize the confidence assigned to predicted classes
#' 
#' @param target An `attribute_target` object with a classification mode.
#' @param on Either `"confidential"` (default, the attack result), `"train"`, 
#' or `"test"` (model-quality diagnostics on the fitting data).
#' 
#' @returns A tibble with columns `source` (`"synthetic"` or `"holdout"`), 
#' `on`, and `confidence` (the average, across records, of the probability 
#' assigned to each record's predicted class).
#' 
#' @export
#' 
target_confidence <- function(target, on = "confidential") {
  
  stopifnot(is_attribute_target(target))
  
  if (target$mode != "classification") {
    
    stop("Error: target_confidence() requires a classification target.")
    
  }
  
  on <- match.arg(on, c("confidential", "train", "test"))
  
  result <- .target_metric_table(
    target = target,
    on = on,
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
