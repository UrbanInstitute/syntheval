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
#' @param source Either `"synthetic"` or `"holdout"` - which fitted branch's 
#' predictions to retrieve.
#' @param on Either `"confidential"`, `"train"`, or `"test"` - which 
#' predictions tibble to retrieve from the branch.
#' 
#' @return A predictions tibble.
#' 
.get_target_predictions <- function(target, source, on) {
  
  branch <- target[[source]]
  
  if (is.null(branch)) {
    
    stop(
      paste0(
        "Error: target does not have a '", source, "' branch. Supply ",
        "eval_data$holdout_data and fit_holdout = TRUE in attribute_target()."
      )
    )
    
  }
  
  if (on == "confidential") {

    predictions <- branch$predictions_confidential

  } else if (on == "train") {
    
    predictions <- branch$predictions_train
    
  } else if (on == "test") {
    
    predictions <- branch$predictions_test
    
  }
  
  return(predictions)
  
}

#' 
#' Compute a metric across every available source of an attribute_target object
#' 
#' @param target An `attribute_target` object.
#' @param on Either `"confidential"`, `"train"`, or `"test"`.
#' @param metric_name The name of the metric column in the returned tibble.
#' @param metric_fn A function that takes a predictions tibble and returns a 
#' single numeric value.
#' 
#' @return A tibble with columns `source`, `on`, and `metric_name`.
#' 
.target_metric_table <- function(target, on, metric_name, metric_fn) {
  
  sources <- .available_sources(target)
  
  values <- purrr::map_dbl(
    .x = sources,
    .f = function(source) {
      
      predictions <- .get_target_predictions(target, source = source, on = on)
      
      return(metric_fn(predictions))
      
    }
  )
  
  result <- tibble::tibble(source = sources, on = on, value = values)
  
  # rename the placeholder "value" column to the caller-supplied metric name
  # (e.g. "accuracy", "rmse") so each target_*() function controls its own
  # output column name without duplicating the tibble-building logic
  names(result)[names(result) == "value"] <- metric_name
  
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
    
  }
  
  # yardstick's multiclass _vec() metrics expect a matrix with one column per
  # level, in level order, instead of a single vector
  return(prob_matrix)
  
}
