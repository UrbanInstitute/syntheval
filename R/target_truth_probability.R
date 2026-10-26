#' 
#' Calculate the probability assigned to the true confidential class
#' 
#' @param target An `attribute_target` object with a classification mode.
#' 
#' @returns A tibble with columns:
#' * `modeling_data`: the data the attack model was fit on, a factor with 
#'   levels `"synthetic"` and, if available, `"holdout"`.
#' * `prediction_data`: the data the truth probability is evaluated on, a 
#'   factor with levels `"training"` and `"testing"` (the internal split of the 
#'   modeling data) and `"confidential"` (the attack result).
#' * `n`: the number of records in `prediction_data`.
#' * `truth_probability`: the average, across records, of the probability 
#'   assigned to each record's true class.
#' 
#' @export
#' 
target_truth_probability <- function(target) {
  
  stopifnot(is_attribute_target(target))
  
  if (target$mode != "classification") {
    
    stop("Error: target_truth_probability() requires a classification target.")
    
  }
  
  result <- .target_metric_grid(
    target = target,
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
