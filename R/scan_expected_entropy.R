#'
#' Expected entropy: population-weighted average of per-class entropy
#'
#' @param attribute_scan An `attribute_scan` object.
#'
#' @returns A tibble with columns `source` (`"synthetic"` or `"holdout"` when
#' holdout data is available), `target_var`, and `expected_entropy` (the
#' population-weighted average of per-class Shannon entropy computed from the
#' source distribution).
#'
#' @export
#'
scan_expected_entropy <- function(attribute_scan) {
  stopifnot(is_attribute_scan(attribute_scan))

  conf_distributions <- attribute_scan$confidential$distributions

  synth_exp_ent <- .expected_entropy_by_target(
    source_distributions = attribute_scan$synthetic$distributions,
    conf_distributions = conf_distributions,
    qid_keys = attribute_scan$qid_keys
  ) |>
    dplyr::mutate(source = "synthetic")

  result_list <- list(synth_exp_ent)

  if (!is.null(attribute_scan$holdout)) {
    holdout_exp_ent <- .expected_entropy_by_target(
      source_distributions = attribute_scan$holdout$distributions,
      conf_distributions = conf_distributions,
      qid_keys = attribute_scan$qid_keys
    ) |>
      dplyr::mutate(source = "holdout")

    result_list <- c(result_list, list(holdout_exp_ent))
  }

  result <- dplyr::bind_rows(result_list) |>
    dplyr::relocate(dplyr::all_of(c("source", "target_var", "expected_entropy")))

  return(result)
}


#'
#' Compute population-weighted expected entropy for each target variable
#'
#' @param source_distributions A long-format conditional distributions tibble
#' for the source (synthetic or holdout) data, as stored in an `attribute_scan`
#' object (see `.conditional_distributions()`).
#' @param conf_distributions A long-format conditional distributions tibble for
#' the confidential data, used to compute `class_n` weights.
#' @param qid_keys A character vector of quasi-identifying column names.
#'
#' @return A tibble with columns `target_var` and `expected_entropy` (the
#' population-weighted average of per-class Shannon entropy computed from the
#' source distribution).
#'
.expected_entropy_by_target <- function(source_distributions, conf_distributions, qid_keys) {

  # per-class entropy from source distribution
  class_entropy <- .entropy_by_class(source_distributions, qid_keys) |>
    dplyr::select(-dplyr::all_of("class_n"))

  class_totals <- conf_distributions |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c("key_id", qid_keys, "target_var")))) |>
    dplyr::summarize(class_n = sum(.data$n), .groups = "drop")

  class_entropy |>
    dplyr::inner_join(class_totals, by = c("key_id", qid_keys, "target_var")) |>
    dplyr::group_by(dplyr::across(dplyr::all_of("target_var"))) |>
    dplyr::summarize(
      expected_entropy = sum(.data$entropy * .data$class_n) / sum(.data$class_n),
      .groups = "drop"
    )
}
