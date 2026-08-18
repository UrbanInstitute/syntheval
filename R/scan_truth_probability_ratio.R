#'
#' Truth probability ratio: synthetic-data truth probability relative to holdout-data truth probability
#'
#' @param attribute_scan An `attribute_scan` object. Must include holdout data
#' (i.e. `attribute_scan$holdout` is not `NULL`).
#'
#' @returns A tibble with columns `target_var`, `synthetic` and `holdout` (the
#' population-weighted average `scan_truth_probability()` value across
#' equivalence classes for each source), and `truth_probability_ratio`
#' (`synthetic` divided by `holdout`). Values above 1 indicate the synthetic
#' data assigns more probability to the true confidential value than the
#' holdout data, suggesting higher attribute inference risk.
#'
#' @export
#'
scan_truth_probability_ratio <- function(attribute_scan) {
  stopifnot(is_attribute_scan(attribute_scan))

  if (is.null(attribute_scan$holdout)) {
    stop(
      "Error: attribute_scan must include holdout data to calculate ",
      "scan_truth_probability_ratio(). Supply holdout_data to eval_data()."
    )
  }

  conf_distributions <- attribute_scan$confidential$distributions

  synth_tp <- .truth_probability_by_target(
    source_distributions = attribute_scan$synthetic$distributions,
    conf_distributions = conf_distributions,
    qid_keys = attribute_scan$qid_keys
  ) |>
    dplyr::mutate(source = "synthetic")

  holdout_tp <- .truth_probability_by_target(
    source_distributions = attribute_scan$holdout$distributions,
    conf_distributions = conf_distributions,
    qid_keys = attribute_scan$qid_keys
  ) |>
    dplyr::mutate(source = "holdout")

  result <- dplyr::bind_rows(synth_tp, holdout_tp) |>
    tidyr::pivot_wider(names_from = "source", values_from = "truth_probability") |>
    dplyr::mutate(truth_probability_ratio = .data$synthetic / .data$holdout) |>
    dplyr::relocate(dplyr::all_of(c("target_var", "synthetic", "holdout", "truth_probability_ratio")))

  return(result)
}


.truth_probability_by_target <- function(source_distributions, conf_distributions, qid_keys) {

  per_class <- .truth_probability_by_class(source_distributions, conf_distributions, qid_keys)

  class_totals <- conf_distributions |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c("key_id", qid_keys, "target_var")))) |>
    dplyr::summarize(class_n = sum(.data$n), .groups = "drop")

  per_class |>
    dplyr::inner_join(class_totals, by = c("key_id", qid_keys, "target_var")) |>
    dplyr::group_by(dplyr::across(dplyr::all_of("target_var"))) |>
    dplyr::summarize(
      truth_probability = sum(.data$truth_probability * .data$class_n) / sum(.data$class_n),
      .groups = "drop"
    )
}
