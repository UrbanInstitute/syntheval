#' 
#' Return the average conditional probability across equivalence classes
#' 
#' @param attribute_scan An `attribute_scan` object.
#' 
#' @returns A tibble with columns `source` (`"confidential"`, `"synthetic"`, or 
#' `"holdout"` when holdout data is available), `target_var`, and 
#' `mean_probability` (the unweighted average, across equivalence classes, of 
#' the highest conditional probability observed for that target variable 
#' within each class).
#' 
#' @export
#' 
scan_mean_probability <- function(attribute_scan) {
  
  stopifnot(is_attribute_scan(attribute_scan))
  
  conf_mean_prob <- .mean_probability_by_target(
    distributions = attribute_scan$confidential$distributions,
    qid_keys = attribute_scan$qid_keys
  ) |>
    dplyr::mutate(source = "confidential")
  
  synth_mean_prob <- .mean_probability_by_target(
    distributions = attribute_scan$synthetic$distributions,
    qid_keys = attribute_scan$qid_keys
  ) |>
    dplyr::mutate(source = "synthetic")
  
  result_list <- list(conf_mean_prob, synth_mean_prob)
  
  # holdout data is optional; only add holdout results when available
  if (!is.null(attribute_scan$holdout)) {
    
    holdout_mean_prob <- .mean_probability_by_target(
      distributions = attribute_scan$holdout$distributions,
      qid_keys = attribute_scan$qid_keys
    ) |>
      dplyr::mutate(source = "holdout")
    
    result_list <- c(result_list, list(holdout_mean_prob))
    
  }
  
  result <- dplyr::bind_rows(result_list) |>
    dplyr::relocate(dplyr::all_of(c("source", "target_var", "mean_probability")))
  
  return(result)
  
}

#' 
#' Compute the per-target-variable average of per-class maximum conditional 
#' probabilities
#' 
#' @param distributions A long-format conditional distributions tibble, as stored
#' in an `attribute_scan` object (see `.conditional_distributions()`).
#' @param qid_keys A character vector of quasi-identifying column names.
#' 
#' @return A tibble with columns `target_var` and `mean_probability`.
#' 
.mean_probability_by_target <- function(distributions, qid_keys) {
  
  .max_probability_by_class(distributions, qid_keys) |>
    dplyr::group_by(dplyr::across(dplyr::all_of("target_var"))) |>
    dplyr::summarize(
      mean_probability = mean(.data$max_probability),
      .groups = "drop"
    )
  
}
