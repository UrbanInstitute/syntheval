#' 
#' Return a population-weighted average conditional probability across 
#' equivalence classes
#' 
#' @param attribute_scan An `attribute_scan` object.
#' 
#' @returns A tibble with columns `source` (`"confidential"`, `"synthetic"`, or 
#' `"holdout"` when holdout data is available), `target_var`, and 
#' `weighted_probability` (the average, across equivalence classes and 
#' weighted by class size, of the highest conditional probability observed for 
#' that target variable within each class).
#' 
#' @export
#' 
scan_weighted_probability <- function(attribute_scan) {
  
  stopifnot(is_attribute_scan(attribute_scan))
  
  conf_weighted_prob <- .weighted_probability_by_target(
    distributions = attribute_scan$confidential$distributions,
    qid_keys = attribute_scan$qid_keys
  ) |>
    dplyr::mutate(source = "confidential")
  
  synth_weighted_prob <- .weighted_probability_by_target(
    distributions = attribute_scan$synthetic$distributions,
    qid_keys = attribute_scan$qid_keys
  ) |>
    dplyr::mutate(source = "synthetic")
  
  result_list <- list(conf_weighted_prob, synth_weighted_prob)
  
  # holdout data is optional; only add holdout results when available
  if (!is.null(attribute_scan$holdout)) {
    
    holdout_weighted_prob <- .weighted_probability_by_target(
      distributions = attribute_scan$holdout$distributions,
      qid_keys = attribute_scan$qid_keys
    ) |>
      dplyr::mutate(source = "holdout")
    
    result_list <- c(result_list, list(holdout_weighted_prob))
    
  }
  
  result <- dplyr::bind_rows(result_list) |>
    dplyr::relocate(dplyr::all_of(c("source", "target_var", "weighted_probability")))
  
  return(result)
  
}

#' 
#' Compute the per-target-variable, class-size-weighted average of per-class 
#' maximum conditional probabilities
#' 
#' @param distributions A long-format conditional distributions tibble, as stored
#' in an `attribute_scan` object (see `.conditional_distributions()`).
#' @param qid_keys A character vector of quasi-identifying column names.
#' 
#' @return A tibble with columns `target_var` and `weighted_probability`.
#' 
.weighted_probability_by_target <- function(distributions, qid_keys) {
  
  max_by_class <- .max_probability_by_class(distributions, qid_keys)
  
  class_totals <- distributions |>
    dplyr::group_by(
      dplyr::across(dplyr::all_of(c("key_id", qid_keys, "target_var")))
    ) |>
    dplyr::summarize(class_n = sum(.data$n), .groups = "drop")
  
  max_by_class |>
    dplyr::inner_join(class_totals, by = c("key_id", qid_keys, "target_var")) |>
    dplyr::group_by(dplyr::across(dplyr::all_of("target_var"))) |>
    dplyr::summarize(
      weighted_probability = sum(.data$max_probability * .data$class_n) / sum(.data$class_n),
      .groups = "drop"
    )
  
}
