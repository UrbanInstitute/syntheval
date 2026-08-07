#' 
#' Quantify uncertainty about a target variable within equivalence classes
#' 
#' @param attribute_scan An `attribute_scan` object.
#' 
#' @returns A tibble with columns `source` (`"confidential"` or `"synthetic"`), 
#' `target_var`, `key_id`, the quasi-identifying key columns, and `value` (the 
#' base-2 Shannon entropy of the conditional target distribution within that 
#' equivalence class).
#' 
#' @export
#' 
scan_entropy <- function(attribute_scan) {
  
  stopifnot(is_attribute_scan(attribute_scan))
  
  conf_entropy <- .entropy_by_class(
    distributions = attribute_scan$confidential$distributions,
    qid_keys = attribute_scan$qid_keys
  ) |>
    dplyr::mutate(source = "confidential")
  
  synth_entropy <- .entropy_by_class(
    distributions = attribute_scan$synthetic$distributions,
    qid_keys = attribute_scan$qid_keys
  ) |>
    dplyr::mutate(source = "synthetic")
  
  result <- dplyr::bind_rows(conf_entropy, synth_entropy) |>
    dplyr::relocate(
      dplyr::all_of(c("source", "target_var", "key_id", attribute_scan$qid_keys, "value"))
    )
  
  return(result)
  
}

#' 
#' Compute per-equivalence-class Shannon entropy from a conditional distributions tibble
#' 
#' @param distributions A long-format conditional distributions tibble, as stored
#' in an `attribute_scan` object (see `.conditional_distributions()`).
#' @param qid_keys A character vector of quasi-identifying column names.
#' 
#' @return A tibble with columns `key_id`, the quasi-identifying key columns,
#' `target_var`, and `value` (base-2 Shannon entropy).
#' 
.entropy_by_class <- function(distributions, qid_keys) {
  
  distributions |>
    dplyr::filter(!is.na(.data$prob)) |>
    dplyr::group_by(
      dplyr::across(dplyr::all_of(c("key_id", qid_keys, "target_var")))
    ) |>
    dplyr::summarise(
      value = -sum(
        dplyr::if_else(.data$prob > 0, .data$prob * log2(.data$prob), 0)
      ),
      .groups = "drop"
    )
  
}
