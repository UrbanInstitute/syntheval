#'
#' Per-class top-class accuracy: probability the top predicted class equals truth
#'
#' @param attribute_scan An `attribute_scan` object.
#'
#' @returns A tibble with columns `source` (`"synthetic"` or `"holdout"` when
#' holdout data is available), `target_var`, `key_id`, the quasi-identifying key
#' columns, and `top_class_accuracy` (the per-class probability that the source
#' distribution's most-probable level equals the confidential value).
#'
#' @export
#'
scan_top_class_accuracy <- function(attribute_scan) {
  stopifnot(is_attribute_scan(attribute_scan))

  conf_distributions <- attribute_scan$confidential$distributions

  synth_acc <- .top_class_accuracy_by_class(
    source_distributions = attribute_scan$synthetic$distributions,
    conf_distributions = conf_distributions,
    qid_keys = attribute_scan$qid_keys
  ) |>
    dplyr::mutate(source = "synthetic")

  result_list <- list(synth_acc)

  if (!is.null(attribute_scan$holdout)) {
    holdout_acc <- .top_class_accuracy_by_class(
      source_distributions = attribute_scan$holdout$distributions,
      conf_distributions = conf_distributions,
      qid_keys = attribute_scan$qid_keys
    ) |>
      dplyr::mutate(source = "holdout")

    result_list <- c(result_list, list(holdout_acc))
  }

  result <- dplyr::bind_rows(result_list) |>
    dplyr::relocate(dplyr::all_of(c("source", "target_var", "key_id", attribute_scan$qid_keys, "top_class_accuracy")))

  return(result)
}


#' 
#' Compute the per-class probability that the source distribution's
#' most-probable level equals the confidential (truth) value
#' 
#' @param source_distributions A long-format conditional distributions tibble
#' for the source (synthetic or holdout) data, as stored in an
#' `attribute_scan` object (see `.conditional_distributions()`).
#' @param conf_distributions A long-format conditional distributions tibble
#' for the confidential data.
#' @param qid_keys A character vector of quasi-identifying column names.
#' 
#' @return A tibble with columns `key_id`, the quasi-identifying key columns,
#' `target_var`, and `top_class_accuracy`.
#' 
.top_class_accuracy_by_class <- function(source_distributions, conf_distributions, qid_keys) {

  # determine top level(s) per class in the source distribution
  top_levels <- source_distributions |>
    dplyr::filter(!is.na(.data$prob)) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c("key_id", qid_keys, "target_var")))) |>
    dplyr::filter(.data$prob == max(.data$prob, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::select(dplyr::all_of(c("key_id", qid_keys, "target_var", "target_level"))) |>
    dplyr::mutate(is_top = TRUE)

  joined <- conf_distributions |>
    dplyr::select(dplyr::all_of(c("key_id", qid_keys, "target_var", "target_level", "prob"))) |>
    dplyr::rename(conf_prob = "prob") |>
    dplyr::left_join(top_levels, by = c("key_id", qid_keys, "target_var", "target_level")) |>
    dplyr::mutate(is_top = dplyr::coalesce(.data$is_top, FALSE))

  result <- joined |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c("key_id", qid_keys, "target_var")))) |>
    dplyr::summarize(
      top_class_accuracy = sum(.data$conf_prob * as.numeric(.data$is_top), na.rm = TRUE),
      .groups = "drop"
    )

  return(result)

}
