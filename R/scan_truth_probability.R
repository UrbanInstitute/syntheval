#'
#' Calculate the probability assigned to the true confidential value by a
#' source distribution (synthetic or holdout).
#'
#' @param attribute_scan An `attribute_scan` object.
#'
#' @returns A tibble with columns `source` (`"synthetic"` or `"holdout"` when
#' holdout data is available), `target_var`, `key_id`, the quasi-identifying key
#' columns, and `truth_probability` (the expected probability assigned by the
#' source distribution to the true confidential value, computed as
#' \eqn{\sum_v p_{conf}(v \mid class)\,p_{source}(v \mid class)}.)
#'
#' @export
#'
scan_truth_probability <- function(attribute_scan) {
  stopifnot(is_attribute_scan(attribute_scan))

  conf_distributions <- attribute_scan$confidential$distributions

  synth_tp <- .truth_probability_by_class(
    source_distributions = attribute_scan$synthetic$distributions,
    conf_distributions = conf_distributions,
    qid_keys = attribute_scan$qid_keys
  ) |>
    dplyr::mutate(source = "synthetic")

  result_list <- list(synth_tp)

  # holdout data is optional; only add holdout results when available
  if (!is.null(attribute_scan$holdout)) {

    holdout_tp <- .truth_probability_by_class(
      source_distributions = attribute_scan$holdout$distributions,
      conf_distributions = conf_distributions,
      qid_keys = attribute_scan$qid_keys
    ) |>
      dplyr::mutate(source = "holdout")

    result_list <- c(result_list, list(holdout_tp))

  }

  result <- dplyr::bind_rows(result_list) |>
    dplyr::relocate(
      dplyr::all_of(
        c("source", "target_var", "key_id", attribute_scan$qid_keys, "truth_probability")
      )
    )

  return(result)

}

#'
#' Compute per-equivalence-class truth probability from conditional distributions
#'
#' @param source_distributions A long-format conditional distributions tibble
#' (synthetic or holdout), as stored in an `attribute_scan` object
#' (see `.conditional_distributions()`).
#' @param conf_distributions A long-format conditional distributions tibble for
#' the confidential data.
#' @param qid_keys A character vector of quasi-identifying column names.
#'
#' @return A tibble with columns `key_id`, the quasi-identifying key columns,
#' `target_var`, and `truth_probability`.
#'
.truth_probability_by_class <- function(source_distributions, conf_distributions, qid_keys) {

  joined <- conf_distributions |>
    dplyr::select(dplyr::all_of(c("key_id", qid_keys, "target_var", "target_level", "prob"))) |>
    dplyr::mutate(conf_prob = .data$prob) |>
    dplyr::select(-dplyr::all_of("prob")) |>
    dplyr::inner_join(
      dplyr::select(source_distributions, dplyr::all_of(c("key_id", qid_keys, "target_var", "target_level", "prob"))) |>
        dplyr::mutate(source_prob = .data$prob) |>
        dplyr::select(-dplyr::all_of("prob")),
      by = c("key_id", "target_var", "target_level", qid_keys)
    )

  # replace any missing probabilities with zero before computing expectation
  joined <- joined |>
    dplyr::mutate(
      conf_prob = dplyr::coalesce(.data$conf_prob, 0),
      source_prob = dplyr::coalesce(.data$source_prob, 0)
    )

  result <- joined |>
    dplyr::group_by(
      dplyr::across(dplyr::all_of(c("key_id", qid_keys, "target_var")))
    ) |>
    dplyr::summarize(
      truth_probability = sum(.data$conf_prob * .data$source_prob, na.rm = TRUE),
      .groups = "drop"
    )

  return(result)

}
