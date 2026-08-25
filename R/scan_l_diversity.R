#' 
#' Measure the diversity of target values within equivalence classes
#' 
#' @param attribute_scan An `attribute_scan` object.
#' @param summarize A logical. If `TRUE` (the default), collapses equivalence
#' classes down to the worst-case (minimum) l-diversity per `source` and
#' `target_var`, mirroring `scan_k_anonymity()`'s use of the minimum class size.
#' If `FALSE`, returns l-diversity for every equivalence class.
#' 
#' @returns If `summarize = TRUE`, a tibble with columns `source`
#' (`"confidential"`, `"synthetic"`, or `"holdout"` when holdout data is
#' available), `target_var`, `key_id`, the quasi-identifying key columns,
#' `class_n`, and `l_diversity`, filtered to the equivalence class with the
#' minimum `l_diversity` for each `source`/`target_var`. If `summarize = FALSE`,
#' the same columns for every equivalence class: `source`, `target_var`,
#' `key_id`, the quasi-identifying key columns, `class_n` (the number of
#' records in that equivalence class), and `l_diversity` (the count of
#' distinct target levels observed within that equivalence class).
#' 
#' @export
#' 
scan_l_diversity <- function(attribute_scan, summarize = TRUE) {
  
  stopifnot(is_attribute_scan(attribute_scan))
  
  conf_l_diversity <- .l_diversity_by_class(
    distributions = attribute_scan$confidential$distributions,
    qid_keys = attribute_scan$qid_keys
  ) |>
    dplyr::mutate(source = "confidential")
  
  synth_l_diversity <- .l_diversity_by_class(
    distributions = attribute_scan$synthetic$distributions,
    qid_keys = attribute_scan$qid_keys
  ) |>
    dplyr::mutate(source = "synthetic")
  
  result_list <- list(conf_l_diversity, synth_l_diversity)
  
  # holdout data is optional; only add holdout results when available
  if (!is.null(attribute_scan$holdout)) {
    
    holdout_l_diversity <- .l_diversity_by_class(
      distributions = attribute_scan$holdout$distributions,
      qid_keys = attribute_scan$qid_keys
    ) |>
      dplyr::mutate(source = "holdout")
    
    result_list <- c(result_list, list(holdout_l_diversity))
    
  }
  
  result <- dplyr::bind_rows(result_list)
  
  # lower l_diversity is riskier, so the worst case is the minimum
  if (summarize) {
    
    result <- result |>
      dplyr::group_by(dplyr::across(dplyr::all_of(c("source", "target_var")))) |>
      dplyr::slice_min(order_by = .data$l_diversity, n = 1, with_ties = FALSE) |>
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
      dplyr::all_of(c("source", "target_var", "key_id", attribute_scan$qid_keys, "class_n", "l_diversity"))
    )
  
  return(result)
  
}

#' 
#' Compute per-equivalence-class l-diversity from a conditional distributions tibble
#' 
#' @param distributions A long-format conditional distributions tibble, as stored
#' in an `attribute_scan` object (see `.conditional_distributions()`).
#' @param qid_keys A character vector of quasi-identifying column names.
#' 
#' @return A tibble with columns `key_id`, the quasi-identifying key columns,
#' `target_var`, `class_n` (records in that equivalence class), and 
#' `l_diversity` (count of distinct observed target levels).
#' 
.l_diversity_by_class <- function(distributions, qid_keys) {
  
  # n > 0 excludes target levels completed by .drop = FALSE but never observed
  distributions |>
    dplyr::filter(.data$n > 0) |>
    dplyr::group_by(
      dplyr::across(dplyr::all_of(c("key_id", qid_keys, "target_var")))
    ) |>
    dplyr::summarize(
      class_n = sum(.data$n),
      l_diversity = dplyr::n_distinct(.data$target_level),
      .groups = "drop"
    )
  
}
