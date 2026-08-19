#' 
#' Quantify uncertainty about a target variable within equivalence classes
#' 
#' @param attribute_scan An `attribute_scan` object.
#' @param summarize A logical. If `TRUE` (the default), collapses equivalence
#' classes down to the worst-case (minimum) entropy per `source` and
#' `target_var`, since lower entropy indicates a more predictable (riskier)
#' equivalence class. If `FALSE`, returns entropy for every equivalence class.
#' 
#' @returns If `summarize = TRUE`, a tibble with columns `source`
#' (`"confidential"`, `"synthetic"`, or `"holdout"` when holdout data is
#' available), `target_var`, `key_id`, the quasi-identifying key columns,
#' `class_n`, `entropy`, and `max_entropy`, filtered to the equivalence class
#' with the minimum `entropy` for each `source`/`target_var`. If
#' `summarize = FALSE`, the same columns for every equivalence class: `source`,
#' `target_var`, `key_id`, the quasi-identifying key columns, `class_n` (the
#' number of records in that equivalence class), `entropy` (the base-2 Shannon
#' entropy of the conditional target distribution within that equivalence
#' class), and `max_entropy` (the maximum possible entropy for that target
#' variable's number of levels, for comparison across target variables).
#' 
#' @export
#' 
scan_entropy <- function(attribute_scan, summarize = TRUE) {
  
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
  
  result <- dplyr::bind_rows(result_list)
  
  # lower entropy is riskier (more predictable), so the worst case is the minimum
  if (summarize) {
    
    result <- result |>
      dplyr::group_by(dplyr::across(dplyr::all_of(c("source", "target_var")))) |>
      dplyr::slice_min(order_by = .data$entropy, n = 1, with_ties = FALSE) |>
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
      dplyr::all_of(
        c("source", "target_var", "key_id", attribute_scan$qid_keys, "class_n", "entropy", "max_entropy")
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
#' `target_var`, `class_n` (records in that equivalence class), `entropy`
#' (base-2 Shannon entropy), and `max_entropy` (the maximum possible entropy 
#' for that target variable's number of levels).
#' 
.entropy_by_class <- function(distributions, qid_keys) {
  
  class_entropy <- distributions |>
    dplyr::filter(!is.na(.data$prob)) |>
    dplyr::group_by(
      dplyr::across(dplyr::all_of(c("key_id", qid_keys, "target_var")))
    ) |>
    dplyr::summarize(
      class_n = sum(.data$n),
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
  
  combined_data <- dplyr::left_join(
    class_entropy, 
    max_entropy, 
    by = "target_var"
  )
  
  return(combined_data)
  
}
