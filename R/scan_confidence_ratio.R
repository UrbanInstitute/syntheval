#'
#' Confidence ratio: synthetic-data confidence relative to holdout-data confidence
#'
#' @param attribute_scan An `attribute_scan` object. Must include holdout data
#' (i.e. `attribute_scan$holdout` is not `NULL`).
#'
#' @returns A tibble with columns `target_var`, `synthetic` and `holdout` (the
#' per-target `scan_confidence()` values for each source), and
#' `confidence_ratio` (`synthetic` divided by `holdout`). Values above 1
#' indicate the synthetic data yields more confident inferences than the
#' holdout data, suggesting higher attribute inference risk.
#'
#' @export
#'
scan_confidence_ratio <- function(attribute_scan) {
  stopifnot(is_attribute_scan(attribute_scan))

  if (is.null(attribute_scan$holdout)) {
    stop(
      "Error: attribute_scan must include holdout data to calculate ",
      "scan_confidence_ratio(). Supply holdout_data to eval_data()."
    )
  }

  confidence <- scan_confidence(attribute_scan)

  result <- confidence |>
    tidyr::pivot_wider(names_from = "source", values_from = "confidence") |>
    dplyr::mutate(confidence_ratio = .data$synthetic / .data$holdout) |>
    dplyr::relocate(dplyr::all_of(c("target_var", "synthetic", "holdout", "confidence_ratio")))

  return(result)
}
