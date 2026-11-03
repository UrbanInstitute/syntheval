#' 
#' Measure the accuracy of predicted class probabilities
#' 
#' @param target An `attribute_target` object with a classification mode.
#' 
#' @returns A tibble with columns:
#' * `modeling_data`: the data the attack model was fit on, a factor with 
#'   levels `"synthetic"` and, if available, `"holdout"`.
#' * `prediction_data`: the data the Brier score is evaluated on, a factor with 
#'   levels `"training"` and `"testing"` (the internal split of the modeling 
#'   data) and `"confidential"` (the attack result).
#' * `n`: the number of records in `prediction_data`.
#' * `brier_score`: the Brier score of the predicted class probabilities, where 
#'   0 is a perfect prediction.
#' 
#' @details 
#' The Brier score uses [yardstick::brier_class_vec()], which scales binary and 
#' multiclass scores identically. Engines that report their own Brier score may 
#' use a different convention - notably, the out-of-bag `prediction.error` of a 
#' `ranger` probability forest sums squared error across classes. Confirm the 
#' two are on the same scale before comparing them.
#' 
#' @export
#' 
target_brier_score <- function(target) {
  
  stopifnot(is_attribute_target(target))
  
  if (target$mode != "classification") {
    
    stop("Error: target_brier_score() requires a classification target.")
    
  }
  
  result <- .target_metric_grid(
    target = target,
    metric_name = "brier_score",
    metric_fn = function(predictions) {
      
      yardstick::brier_class_vec(
        truth = predictions[[target$target_var]],
        estimate = .class_prob_estimate(predictions, target$target_var)
      )
      
    }
  )
  
  return(result)
  
}
