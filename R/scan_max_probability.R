#' 
#' Return the highest conditional probability observed within each equivalence class
#' 
#' @param attribute_scan An `attribute_scan` object.
#' 
#' @returns A tibble with columns `source` (`"confidential"` or `"synthetic"`), 
#' `target_var`, `key_id`, the quasi-identifying key columns, and 
#' `max_probability` (the highest conditional probability of any target level 
#' within that equivalence class).
#' 
#' @export
#' 
scan_max_probability <- function(attribute_scan) {
  
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
  
  result <- dplyr::bind_rows(conf_max_prob, synth_max_prob) |>
    dplyr::relocate(
      dplyr::all_of(c("source", "target_var", "key_id", attribute_scan$qid_keys, "max_probability"))
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
#' `target_var`, and `max_probability`.
#' 
.max_probability_by_class <- function(distributions, qid_keys) {
  
  distributions |>
    dplyr::filter(!is.na(.data$prob)) |>
    dplyr::group_by(
      dplyr::across(dplyr::all_of(c("key_id", qid_keys, "target_var")))
    ) |>
    dplyr::summarize(
      max_probability = max(.data$prob),
      .groups = "drop"
    )
  
}
