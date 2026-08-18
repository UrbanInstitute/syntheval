#' 
#' Return the highest conditional probability observed within each equivalence class
#' 
#' @param attribute_scan An `attribute_scan` object.
#' @param summarize A logical. If `TRUE` (the default), collapses equivalence
#' classes down to the worst-case (maximum) `max_probability` per `source` and
#' `target_var`, since a higher probability indicates a more confident (riskier)
#' inference. If `FALSE`, returns `max_probability` for every equivalence class.
#' 
#' @returns If `summarize = TRUE`, a tibble with columns `source`
#' (`"confidential"`, `"synthetic"`, or `"holdout"` when holdout data is
#' available), `target_var`, `key_id`, the quasi-identifying key columns,
#' `class_n`, and `max_probability`, filtered to the equivalence class with the
#' maximum `max_probability` for each `source`/`target_var`. If
#' `summarize = FALSE`, the same columns for every equivalence class: `source`,
#' `target_var`, `key_id`, the quasi-identifying key columns, `class_n` (the
#' number of records in that equivalence class), and `max_probability` (the
#' highest conditional probability of any target level within that equivalence
#' class).
#' 
#' @export
#' 
scan_max_probability <- function(attribute_scan, summarize = TRUE) {
  
  stopifnot(is_attribute_scan(attribute_scan))
  
  conf_max_prob <- .max_probability_by_class(
    distributions = attribute_scan$confidential$distributions,
    qid_keys = attribute_scan$qid_keys
  ) |>
    dplyr::mutate(source = "confidential")
  
  synth_max_prob <- .max_probability_by_class(
    distributions = attribute_scan$synthetic$distributions,
    qid_keys = attribute_scan$qid_keys
  ) |>
    dplyr::mutate(source = "synthetic")
  
  result_list <- list(conf_max_prob, synth_max_prob)
  
  # holdout data is optional; only add holdout results when available
  if (!is.null(attribute_scan$holdout)) {
    
    holdout_max_prob <- .max_probability_by_class(
      distributions = attribute_scan$holdout$distributions,
      qid_keys = attribute_scan$qid_keys
    ) |>
      dplyr::mutate(source = "holdout")
    
    result_list <- c(result_list, list(holdout_max_prob))
    
  }
  
  result <- dplyr::bind_rows(result_list)
  
  # higher max_probability is riskier (more confident inference), so the worst case is the maximum
  if (summarize) {
    
    result <- result |>
      dplyr::group_by(dplyr::across(dplyr::all_of(c("source", "target_var")))) |>
      dplyr::slice_max(order_by = .data$max_probability, n = 1, with_ties = FALSE) |>
      dplyr::ungroup()
    
  }
  
  result <- result |>
    # group confidential/synthetic/holdout rows together within each target_var
    dplyr::arrange(
      .data$target_var,
      match(.data$source, c("confidential", "synthetic", "holdout")),
      .data$key_id
    ) |>
    dplyr::relocate(
      dplyr::all_of(c("source", "target_var", "key_id", attribute_scan$qid_keys, "class_n", "max_probability"))
    )
  
  return(result)
  
}

#' 
#' Compute the per-equivalence-class maximum conditional probability from a 
#' conditional distributions tibble
#' 
#' @param distributions A long-format conditional distributions tibble, as stored
#' in an `attribute_scan` object (see `.conditional_distributions()`).
#' @param qid_keys A character vector of quasi-identifying column names.
#' 
#' @return A tibble with columns `key_id`, the quasi-identifying key columns,
#' `target_var`, `class_n` (records in that equivalence class), and 
#' `max_probability`.
#' 
.max_probability_by_class <- function(distributions, qid_keys) {
  
  distributions |>
    dplyr::filter(!is.na(.data$prob)) |>
    dplyr::group_by(
      dplyr::across(dplyr::all_of(c("key_id", qid_keys, "target_var")))
    ) |>
    dplyr::summarize(
      class_n = sum(.data$n),
      max_probability = max(.data$prob),
      .groups = "drop"
    )
  
}
