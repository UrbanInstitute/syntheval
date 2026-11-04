#' 
#' Character vector of the branches available on an attribute_target object
#' 
#' @param target An `attribute_target` object.
#' 
#' @return A character vector, `"synthetic"`, or `c("synthetic", "holdout")` 
#' when a holdout branch is available.
#' 
.available_sources <- function(target) {
  
  sources <- "synthetic"
  
  if (!is.null(target$holdout)) {
    
    sources <- c(sources, "holdout")
    
  }
  
  return(sources)
  
}

#' 
#' Retrieve a predictions tibble from an attribute_target object
#' 
#' @param target An `attribute_target` object.
#' @param modeling_data Either `"synthetic"` or `"holdout"` - which fitted 
#' branch's predictions to retrieve.
#' @param prediction_data Either `"training"`, `"testing"`, or 
#' `"confidential"` - which predictions tibble to retrieve from the branch.
#' 
#' @return A predictions tibble.
#' 
.get_target_predictions <- function(target, modeling_data, prediction_data) {
  
  branch <- target[[modeling_data]]
  
  if (is.null(branch)) {
    
    stop(
      paste0(
        "Error: target does not have a '", modeling_data, "' branch. Supply ",
        "eval_data$holdout_data and fit_holdout = TRUE in attribute_target()."
      )
    )
    
  }
  
  if (prediction_data == "confidential") {

    predictions <- branch$predictions_confidential

  } else if (prediction_data == "training") {
    
    predictions <- branch$predictions_train
    
  } else if (prediction_data == "testing") {
    
    predictions <- branch$predictions_test
    
  }
  
  return(predictions)
  
}

#' 
#' Compute a metric for every modeling data and prediction data combination of 
#' an attribute_target object
#' 
#' @param target An `attribute_target` object.
#' @param metric_name The name of the metric column in the returned tibble.
#' @param metric_fn A function that takes a predictions tibble and returns a 
#' single numeric value.
#' 
#' @return A tibble with columns `modeling_data` (a factor ordered 
#' `"synthetic"`, `"holdout"`), `prediction_data` (a factor ordered 
#' `"training"`, `"testing"`, `"confidential"`), `n` (the number of records 
#' predicted), and `metric_name`.
#' 
.target_metric_grid <- function(target, metric_name, metric_fn) {
  
  modeling_levels <- .available_sources(target)
  prediction_levels <- c("training", "testing", "confidential")
  
  result <- tidyr::expand_grid(
    modeling_data = factor(modeling_levels, levels = modeling_levels),
    prediction_data = factor(prediction_levels, levels = prediction_levels)
  )
  
  predictions <- purrr::map2(
    .x = as.character(result$modeling_data),
    .y = as.character(result$prediction_data),
    .f = function(modeling_data, prediction_data) {
      
      .get_target_predictions(
        target, 
        modeling_data = modeling_data, 
        prediction_data = prediction_data
      )
      
    }
  )
  
  result$n <- purrr::map_int(predictions, nrow)
  result[[metric_name]] <- purrr::map_dbl(predictions, metric_fn)
  
  return(result)
  
}

#' 
#' Class probability estimate(s) for a predictions tibble, for use with 
#' yardstick `_vec()` prob-based metrics
#' 
#' @param predictions A predictions tibble with a truth column and `.pred_*` 
#' class probability columns.
#' @param target_var The name of the truth column.
#' 
#' @return A numeric vector (binary classification, probability of the first 
#' level) or a numeric matrix (multiclass classification, one column per 
#' level, in level order).
#' 
.class_prob_estimate <- function(predictions, target_var) {
  
  truth <- predictions[[target_var]]
  levels_truth <- levels(truth)
  
  # one column of predicted probability per class level, one row per record
  prob_matrix <- as.matrix(predictions[paste0(".pred_", levels_truth)])
  
  if (length(levels_truth) == 2) {
    
    # yardstick's binary _vec() metrics expect a single probability vector for
    # the "event" level, which defaults to the first factor level
    return(prob_matrix[, 1])
    
  } else {
    
    # yardstick's multiclass _vec() metrics expect a matrix with one column per
    # level, in level order, instead of a single vector
    return(prob_matrix)
    
  }
    
}
