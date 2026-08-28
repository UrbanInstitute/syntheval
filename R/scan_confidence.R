#'
#' Confidence: population-weighted average of per-class maximum source probability
#'
#' @param attribute_scan An `attribute_scan` object.
#'
#' @returns A tibble with columns `source` (`"synthetic"` or `"holdout"` when
#' holdout data is available), `target_var`, and `confidence` (the
#' population-weighted average of the per-class maximum probability assigned by
#' the source distribution).
#'
#' @export
#'
scan_confidence <- function(attribute_scan) {
  stopifnot(is_attribute_scan(attribute_scan))

  conf_distributions <- attribute_scan$confidential$distributions

  synth_conf <- .confidence_by_target(
    source_distributions = attribute_scan$synthetic$distributions,
    conf_distributions = conf_distributions,
    qid_keys = attribute_scan$qid_keys
  ) |>
    dplyr::mutate(source = "synthetic")

  result_list <- list(synth_conf)

  if (!is.null(attribute_scan$holdout)) {
    holdout_conf <- .confidence_by_target(
      source_distributions = attribute_scan$holdout$distributions,
      conf_distributions = conf_distributions,
      qid_keys = attribute_scan$qid_keys
    ) |>
      dplyr::mutate(source = "holdout")

    result_list <- c(result_list, list(holdout_conf))
  }

  result <- dplyr::bind_rows(result_list) |>
    dplyr::relocate(dplyr::all_of(c("source", "target_var", "confidence")))

  return(result)
}


#'
#' Compute population-weighted confidence for each target variable
#'
#' @param source_distributions A long-format conditional distributions tibble
#' for the source (synthetic or holdout) data, as stored in an `attribute_scan`
#' object (see `.conditional_distributions()`).
#' @param conf_distributions A long-format conditional distributions tibble for
#' the confidential data, used to compute `class_n` weights.
#' @param qid_keys A character vector of quasi-identifying column names.
#'
#' @return A tibble with columns `target_var` and `confidence` (the
#' population-weighted average of the per-class maximum probability assigned by
#' the source distribution).
#'
.confidence_by_target <- function(source_distributions, conf_distributions, qid_keys) {

  # max probability per class from source
  max_by_class <- .max_probability_by_class(distributions = source_distributions, qid_keys = qid_keys) |>
    dplyr::select(-dplyr::all_of("class_n"))

  class_totals <- conf_distributions |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c("key_id", qid_keys, "target_var")))) |>
    dplyr::summarize(class_n = sum(.data$n), .groups = "drop")

  max_by_class |>
    dplyr::inner_join(class_totals, by = c("key_id", qid_keys, "target_var")) |>
    dplyr::group_by(dplyr::across(dplyr::all_of("target_var"))) |>
    dplyr::summarize(
      confidence = sum(.data$max_probability * .data$class_n) / sum(.data$class_n),
      .groups = "drop"
    )
}
