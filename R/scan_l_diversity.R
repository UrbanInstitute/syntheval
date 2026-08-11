#' 
#' Measure the diversity of target values within equivalence classes
#' 
#' @param attribute_scan An `attribute_scan` object.
#' 
#' @returns A tibble with columns `source` (`"confidential"` or `"synthetic"`), 
#' `target_var`, `key_id`, the quasi-identifying key columns, and `l_diversity` 
#' (the count of distinct target levels observed within that equivalence class).
#' 
#' @export
#' 
scan_l_diversity <- function(attribute_scan) {
  
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
  
  result <- dplyr::bind_rows(conf_l_diversity, synth_l_diversity) |>
    dplyr::relocate(
      dplyr::all_of(c("source", "target_var", "key_id", attribute_scan$qid_keys, "l_diversity"))
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
#' `target_var`, and `l_diversity` (count of distinct observed target levels).
#' 
.l_diversity_by_class <- function(distributions, qid_keys) {
  
  # n > 0 excludes target levels completed by .drop = FALSE but never observed
  distributions |>
    dplyr::filter(.data$n > 0) |>
    dplyr::group_by(
      dplyr::across(dplyr::all_of(c("key_id", qid_keys, "target_var")))
    ) |>
    dplyr::summarize(
      l_diversity = dplyr::n_distinct(.data$target_level),
      .groups = "drop"
    )
  
}
