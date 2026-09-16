#' 
#' Calculate the probability assigned to the true confidential class
#' 
#' @param target An `attribute_target` object with a classification mode.
#' @param on Either `"confidential"` (default, the attack result), `"train"`, 
#' or `"test"` (model-quality diagnostics on the fitting data).
#' 
#' @returns A tibble with columns `source` (`"synthetic"` or `"holdout"`), 
#' `on`, and `truth_probability` (the average, across records, of the 
#' probability assigned to each record's true class).
#' 
#' @export
#' 
target_truth_probability <- function(target, on = "confidential") {
  
  stopifnot(is_attribute_target(target))
  
  if (target$mode != "classification") {
    
    stop("Error: target_truth_probability() requires a classification target.")
    
  }
  
  on <- match.arg(on, c("confidential", "train", "test"))
  
  result <- .target_metric_table(
    target = target,
    on = on,
    metric_name = "truth_probability",
    metric_fn = function(predictions) {
      
      truth <- predictions[[target$target_var]]
      levels_truth <- levels(truth)
      
      # one probability column per class level, renamed to match the level
      # itself so it can be looked up by name below
      prob_df <- predictions[paste0(".pred_", levels_truth)]
      names(prob_df) <- levels_truth
      
      # for each record, pull the probability of its own true class by name
      # instead of building a matrix and indexing it by (row, col) pairs
      true_class_prob <- purrr::map2_dbl(
        as.character(truth),
        seq_along(truth),
        function(level, row) prob_df[[level]][row]
      )
      
      mean(true_class_prob)
      
    }
  )
  
  return(result)
  
}
