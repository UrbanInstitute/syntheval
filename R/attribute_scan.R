#' 
#' Create an attribute_scan object for non-parametric discovery attack metrics
#' 
#' @param eval_data An `eval_data` object.
#' @param qid_keys A character vector of quasi-identifying keys. Must be `factor` 
#' type variables present in both the confidential and synthetic data.
#' @param target_keys An optional character vector of target variables of interest. 
#' Must be disjoint from `qid_keys` and `factor` type. Defaults to the complement 
#' of `qid_keys` among the factor columns of `eval_data$conf_data`.
#' 
#' @returns An `attribute_scan` object. If `eval_data$holdout_data` is supplied, 
#' the object also includes a `holdout` element with the same structure as 
#' `confidential` and `synthetic`.
#' 
#' @export
#' 
attribute_scan <- function(eval_data, qid_keys, target_keys = NULL) {
  
  stopifnot(is_eval_data(eval_data))
  
  if (eval_data$n_rep > 1) {
    
    synth_data <- eval_data$synth_data[[1]]
    message("Creating attribute_scan object using 1 synthetic data replicate.")
    
  } else {
    
    synth_data <- eval_data$synth_data
    
  }
  
  conf_data <- eval_data$conf_data
  conf_data_types <- unlist(purrr::map(conf_data, pillar::type_sum))
  
  # require factor types for qid_keys
  stopifnot(length(qid_keys) > 0)
  stopifnot(all(qid_keys %in% names(conf_data)))
  stopifnot(all(conf_data_types[qid_keys] == "fct"))
  
  # if no target_keys provided, use the complement of qid_keys among factor columns
  if (is.null(target_keys)) {
    
    target_keys <- setdiff(
      names(conf_data)[conf_data_types == "fct"], 
      qid_keys
    )
    
  }
  
  # require factor types for target_keys and no overlap with qid_keys
  stopifnot(length(target_keys) > 0)
  stopifnot(all(target_keys %in% names(conf_data)))
  stopifnot(all(conf_data_types[target_keys] == "fct"))
  stopifnot(length(intersect(qid_keys, target_keys)) == 0)
  
  # validate keys are present and consistent (factor levels) across data sets
  .validate_eval_keys(eval_data, keys = c(qid_keys, target_keys))
  
  result <- list(
    qid_keys = qid_keys,
    target_keys = target_keys,
    synthetic = list(
      equivalence_classes = .aggregate_qid(synth_data, keys = qid_keys),
      distributions = .conditional_distributions(
        synth_data, 
        qid_keys = qid_keys, 
        target_keys = target_keys
      )
    ),
    confidential = list(
      equivalence_classes = .aggregate_qid(conf_data, keys = qid_keys),
      distributions = .conditional_distributions(
        conf_data, 
        qid_keys = qid_keys, 
        target_keys = target_keys
      )
    )
  )
  
  # holdout data is optional; only compute equivalence classes/distributions 
  # when eval_data$holdout_data is supplied
  if (!is.null(eval_data$holdout_data)) {
    
    holdout_data <- eval_data$holdout_data
    
    result$holdout <- list(
      equivalence_classes = .aggregate_qid(holdout_data, keys = qid_keys),
      distributions = .conditional_distributions(
        holdout_data, 
        qid_keys = qid_keys, 
        target_keys = target_keys
      )
    )
    
  }
  
  result$call <- match.call()
  
  result <- structure(result, class = "attribute_scan")
  
  return(result)
  
}

#' 
#' Check if object is `attribute_scan`
#' 
#' @param x object
#' 
#' @return A boolean
#' 
#' @export
#' 
is_attribute_scan <- function(x) {
  inherits(x, "attribute_scan")
}

#' @export
print.attribute_scan <- function(x, ...) {
  
  cat("Attribute Scan \n")
  cat("Quasi-Identifiers: ", paste(x$qid_keys, collapse = ", "), "\n")
  cat("Target Variables: ", paste(x$target_keys, collapse = ", "), "\n")
  
  cat(
    "Confidential Equivalence Classes: ", 
    nrow(x$confidential$equivalence_classes), 
    "\n"
  )
  
  cat(
    "Synthetic Equivalence Classes: ", 
    nrow(x$synthetic$equivalence_classes), 
    "\n"
  )
  
  if (!is.null(x$holdout)) {
    
    cat(
      "Holdout Equivalence Classes: ", 
      nrow(x$holdout$equivalence_classes), 
      "\n"
    )
    
  }
  
  invisible(x)
  
}
