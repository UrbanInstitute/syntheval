#' 
#' Measure how closely each equivalence class target distribution matches the 
#' overall target distribution
#' 
#' @param attribute_scan An `attribute_scan` object.
#' @param metric A string describing the distance metric between the equivalence 
#' class and overall target distributions. One of `"linf"` (L-infinity distance), 
#' `"l1"` (L1 distance), or `"l2"` (L2 distance), defaults to `"linf"`.
#' @param summarize A logical. If `TRUE` (the default), collapses equivalence
#' classes down to the worst-case (maximum) t-closeness per `source` and
#' `target_var`, since higher t-closeness indicates greater divergence from the
#' overall target distribution. If `FALSE`, returns t-closeness for every
#' equivalence class.
#' 
#' @returns If `summarize = TRUE`, a tibble with columns `source`
#' (`"confidential"`, `"synthetic"`, or `"holdout"` when holdout data is
#' available), `target_var`, `key_id`, the quasi-identifying key columns,
#' `class_n`, and `t_closeness`, filtered to the equivalence class with the
#' maximum `t_closeness` for each `source`/`target_var`. If `summarize = FALSE`,
#' the same columns for every equivalence class: `source`, `target_var`,
#' `key_id`, the quasi-identifying key columns, `class_n` (the number of
#' records in that equivalence class), and `t_closeness` (the distance between
#' the equivalence class and overall target distributions).
#' 
#' @export
#' 
scan_t_closeness <- function(attribute_scan, metric = c("linf", "l1", "l2"), summarize = TRUE) {
  
  stopifnot(is_attribute_scan(attribute_scan))
  metric <- match.arg(metric)
  
  conf_t_closeness <- .t_closeness_by_class(
    distributions = attribute_scan$confidential$distributions,
    qid_keys = attribute_scan$qid_keys,
    metric = metric
  ) |>
    dplyr::mutate(source = "confidential")
  
  synth_t_closeness <- .t_closeness_by_class(
    distributions = attribute_scan$synthetic$distributions,
    qid_keys = attribute_scan$qid_keys,
    metric = metric
  ) |>
    dplyr::mutate(source = "synthetic")
  
  result_list <- list(conf_t_closeness, synth_t_closeness)
  
  # holdout data is optional; only add holdout results when available
  if (!is.null(attribute_scan$holdout)) {
    
    holdout_t_closeness <- .t_closeness_by_class(
      distributions = attribute_scan$holdout$distributions,
      qid_keys = attribute_scan$qid_keys,
      metric = metric
    ) |>
      dplyr::mutate(source = "holdout")
    
    result_list <- c(result_list, list(holdout_t_closeness))
    
  }
  
  result <- dplyr::bind_rows(result_list)
  
  # higher t_closeness is riskier, so the worst case is the maximum
  if (summarize) {
    
    result <- result |>
      dplyr::group_by(dplyr::across(dplyr::all_of(c("source", "target_var")))) |>
      dplyr::slice_max(order_by = .data$t_closeness, n = 1, with_ties = FALSE) |>
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
      dplyr::all_of(c("source", "target_var", "key_id", attribute_scan$qid_keys, "class_n", "t_closeness"))
    )
  
  return(result)
  
}

#' 
#' Compute per-equivalence-class t-closeness from a conditional distributions tibble
#' 
#' @param distributions A long-format conditional distributions tibble, as stored
#' in an `attribute_scan` object (see `.conditional_distributions()`).
#' @param qid_keys A character vector of quasi-identifying column names.
#' @param metric A string, one of `"linf"`, `"l1"`, or `"l2"`.
#' 
#' @return A tibble with columns `key_id`, the quasi-identifying key columns,
#' `target_var`, `class_n` (records in that equivalence class), and 
#' `t_closeness` (distance from the overall target distribution).
#' 
.t_closeness_by_class <- function(distributions, qid_keys, metric) {
  
  # equivalence classes partition all records, so summing n across every class
  # for a target_level reconstructs its overall (unconditional) count
  overall_dist <- distributions |>
    dplyr::group_by(
      dplyr::across(dplyr::all_of(c("target_var", "target_level")))
    ) |>
    dplyr::summarize(overall_n = sum(.data$n), .groups = "drop") |>
    dplyr::group_by(dplyr::across(dplyr::all_of("target_var"))) |>
    dplyr::mutate(overall_prob = .data$overall_n / sum(.data$overall_n)) |>
    dplyr::ungroup() |>
    dplyr::select(dplyr::all_of(c("target_var", "target_level", "overall_prob")))
  
  by_class <- distributions |>
    dplyr::left_join(overall_dist, by = c("target_var", "target_level")) |>
    dplyr::mutate(abs_diff = abs(.data$prob - .data$overall_prob)) |>
    dplyr::group_by(
      dplyr::across(dplyr::all_of(c("key_id", qid_keys, "target_var")))
    )
  
  result <- switch(
    metric,
    "l1" = dplyr::summarize(by_class, class_n = sum(.data$n), t_closeness = sum(.data$abs_diff), .groups = "drop"),
    "l2" = dplyr::summarize(by_class, class_n = sum(.data$n), t_closeness = sqrt(sum(.data$abs_diff ^ 2)), .groups = "drop"),
    "linf" = dplyr::summarize(by_class, class_n = sum(.data$n), t_closeness = max(.data$abs_diff), .groups = "drop")
  )
  
  return(result)
  
}
