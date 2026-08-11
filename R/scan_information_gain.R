#' 
#' Measure how much information the quasi-identifiers reveal about a target 
#' variable
#' 
#' @param attribute_scan An `attribute_scan` object.
#' 
#' @returns A tibble with columns `source` (`"confidential"`, `"synthetic"`, or 
#' `"holdout"` when holdout data is available), `target_var`, and 
#' `information_gain` (the reduction, in base-2 Shannon entropy, from the 
#' overall target distribution to the class-size-weighted average conditional 
#' target distribution within equivalence classes).
#' 
#' @export
#' 
scan_information_gain <- function(attribute_scan) {
  
  stopifnot(is_attribute_scan(attribute_scan))
  
  conf_info_gain <- .information_gain_by_target(
    distributions = attribute_scan$confidential$distributions,
    qid_keys = attribute_scan$qid_keys
  ) |>
    dplyr::mutate(source = "confidential")
  
  synth_info_gain <- .information_gain_by_target(
    distributions = attribute_scan$synthetic$distributions,
    qid_keys = attribute_scan$qid_keys
  ) |>
    dplyr::mutate(source = "synthetic")
  
  result_list <- list(conf_info_gain, synth_info_gain)
  
  # holdout data is optional; only add holdout results when available
  if (!is.null(attribute_scan$holdout)) {
    
    holdout_info_gain <- .information_gain_by_target(
      distributions = attribute_scan$holdout$distributions,
      qid_keys = attribute_scan$qid_keys
    ) |>
      dplyr::mutate(source = "holdout")
    
    result_list <- c(result_list, list(holdout_info_gain))
    
  }
  
  result <- dplyr::bind_rows(result_list) |>
    dplyr::relocate(dplyr::all_of(c("source", "target_var", "information_gain")))
  
  return(result)
  
}

#' 
#' Compute the per-target-variable information gain from a conditional 
#' distributions tibble
#' 
#' @param distributions A long-format conditional distributions tibble, as stored
#' in an `attribute_scan` object (see `.conditional_distributions()`).
#' @param qid_keys A character vector of quasi-identifying column names.
#' 
#' @return A tibble with columns `target_var` and `information_gain`.
#' 
.information_gain_by_target <- function(distributions, qid_keys) {
  
  # overall (unconditional) entropy of each target variable, reconstructed by 
  # summing n across every equivalence class per target_level
  # H(target) = -sum_{target_levels} p * log2(p)
  overall_entropy <- distributions |>
    dplyr::group_by(
      dplyr::across(dplyr::all_of(c("target_var", "target_level")))
    ) |>
    dplyr::summarize(overall_n = sum(.data$n), .groups = "drop") |>
    dplyr::group_by(dplyr::across(dplyr::all_of("target_var"))) |>
    dplyr::mutate(overall_prob = .data$overall_n / sum(.data$overall_n)) |>
    dplyr::summarize(
      entropy = -sum(
        dplyr::if_else(.data$overall_prob > 0, .data$overall_prob * log2(.data$overall_prob), 0)
      ),
      .groups = "drop"
    )
  
  # class-size-weighted average conditional entropy of each target variable
  # H(target | QIs) = sum_{classes} (class_n / total_n) * H(target | class)
  class_entropy <- .entropy_by_class(distributions, qid_keys)
  
  class_totals <- distributions |>
    dplyr::group_by(
      dplyr::across(dplyr::all_of(c("key_id", qid_keys, "target_var")))
    ) |>
    dplyr::summarize(class_n = sum(.data$n), .groups = "drop")
  
  conditional_entropy <- class_entropy |>
    dplyr::inner_join(class_totals, by = c("key_id", qid_keys, "target_var")) |>
    dplyr::group_by(dplyr::across(dplyr::all_of("target_var"))) |>
    dplyr::summarize(
      entropy = sum(.data$entropy * .data$class_n) / sum(.data$class_n),
      .groups = "drop"
    )
  
  # IG(target | QIs) = H(target) - H(target | QIs)
  result <- overall_entropy |>
    dplyr::inner_join(
      conditional_entropy, by = "target_var", suffix = c("_overall", "_conditional")
    ) |>
    dplyr::mutate(information_gain = .data$entropy_overall - .data$entropy_conditional) |>
    dplyr::select(dplyr::all_of(c("target_var", "information_gain")))
  
  return(result)
  
}
