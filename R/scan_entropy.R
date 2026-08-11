#' 
#' Quantify uncertainty about a target variable within equivalence classes
#' 
#' @param attribute_scan An `attribute_scan` object.
#' 
#' @returns A tibble with columns `source` (`"confidential"`, `"synthetic"`, or 
#' `"holdout"` when holdout data is available), `target_var`, `key_id`, the 
#' quasi-identifying key columns, `entropy` (the base-2 Shannon entropy of the 
#' conditional target distribution within that equivalence class), and `max_entropy` 
#' (the maximum possible entropy for that target variable's number of levels, 
#' for comparison across target variables).
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
  
  result_list <- list(conf_entropy, synth_entropy)
  
  # holdout data is optional; only add holdout results when available
  if (!is.null(attribute_scan$holdout)) {
    
    holdout_entropy <- .entropy_by_class(
      distributions = attribute_scan$holdout$distributions,
      qid_keys = attribute_scan$qid_keys
    ) |>
      dplyr::mutate(source = "holdout")
    
    result_list <- c(result_list, list(holdout_entropy))
    
  }
  
  result <- dplyr::bind_rows(result_list) |>
    dplyr::relocate(
      dplyr::all_of(
        c("source", "target_var", "key_id", attribute_scan$qid_keys, "entropy", "max_entropy")
      )
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
#' `target_var`, `entropy` (base-2 Shannon entropy), and `max_entropy` (the 
#' maximum possible entropy for that target variable's number of levels).
#' 
.entropy_by_class <- function(distributions, qid_keys) {
  
  class_entropy <- distributions |>
    dplyr::filter(!is.na(.data$prob)) |>
    dplyr::group_by(
      dplyr::across(dplyr::all_of(c("key_id", qid_keys, "target_var")))
    ) |>
    dplyr::summarize(
      entropy = -sum(
        dplyr::if_else(.data$prob > 0, .data$prob * log2(.data$prob), 0)
      ),
      .groups = "drop"
    )
  
  # .drop = FALSE completes distributions against every declared target level, 
  # so n_distinct(target_level) reflects the target variable's full level count
  max_entropy <- distributions |>
    dplyr::group_by(dplyr::across(dplyr::all_of("target_var"))) |>
    dplyr::summarize(
      max_entropy = log2(dplyr::n_distinct(.data$target_level)),
      .groups = "drop"
    )
  
  return(dplyr::left_join(class_entropy, max_entropy, by = "target_var"))
  
}
