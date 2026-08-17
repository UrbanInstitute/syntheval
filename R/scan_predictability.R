#'
#' Predictability: population-weighted accuracy of top-class inference
#'
#' @param attribute_scan An `attribute_scan` object.
#'
#' @returns A tibble with columns `source` (`"synthetic"` or `"holdout"` when
#' holdout data is available), `target_var`, and `predictability` (the
#' population-weighted average, across records, of per-class top-class
#' accuracy).
#'
#' @export
#'
scan_predictability <- function(attribute_scan) {
  stopifnot(is_attribute_scan(attribute_scan))

  conf_distributions <- attribute_scan$confidential$distributions

  synth_pred <- .predictability_by_target(
    source_distributions = attribute_scan$synthetic$distributions,
    conf_distributions = conf_distributions,
    qid_keys = attribute_scan$qid_keys
  ) |>
    dplyr::mutate(source = "synthetic")

  result_list <- list(synth_pred)

  if (!is.null(attribute_scan$holdout)) {
    holdout_pred <- .predictability_by_target(
      source_distributions = attribute_scan$holdout$distributions,
      conf_distributions = conf_distributions,
      qid_keys = attribute_scan$qid_keys
    ) |>
      dplyr::mutate(source = "holdout")

    result_list <- c(result_list, list(holdout_pred))
  }

  result <- dplyr::bind_rows(result_list) |>
    dplyr::relocate(dplyr::all_of(c("source", "target_var", "predictability")))

  return(result)
}


.predictability_by_target <- function(source_distributions, conf_distributions, qid_keys) {

  per_class <- .top_class_accuracy_by_class(source_distributions, conf_distributions, qid_keys)

  class_totals <- conf_distributions |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c("key_id", qid_keys, "target_var")))) |>
    dplyr::summarize(class_n = sum(.data$n), .groups = "drop")

  per_class |>
    dplyr::inner_join(class_totals, by = c("key_id", qid_keys, "target_var")) |>
    dplyr::group_by(dplyr::across(dplyr::all_of("target_var"))) |>
    dplyr::summarize(
      predictability = sum(.data$top_class_accuracy * .data$class_n) / sum(.data$class_n),
      .groups = "drop"
    )
}
