#'
#' Excess confidence: increase in confidence attributable to the synthetic data
#'
#' @param attribute_scan An `attribute_scan` object. Must include holdout data
#' (i.e. `attribute_scan$holdout` is not `NULL`).
#'
#' @returns A tibble with columns `target_var`, `synthetic` and `holdout` (the
#' per-target `scan_confidence()` values for each source), and
#' `excess_confidence` (`synthetic` minus `holdout`). Values above 0 indicate
#' the synthetic data yields more confident inferences than the holdout data,
#' suggesting higher attribute inference risk attributable to the synthetic
#' data itself rather than superpopulation characteristics.
#'
#' @export
#'
scan_excess_confidence <- function(attribute_scan) {
  stopifnot(is_attribute_scan(attribute_scan))

  if (is.null(attribute_scan$holdout)) {
    stop(
      "Error: attribute_scan must include holdout data to calculate ",
      "scan_excess_confidence(). Supply holdout_data to eval_data()."
    )
  }

  confidence <- scan_confidence(attribute_scan)

  result <- confidence |>
    tidyr::pivot_wider(names_from = "source", values_from = "confidence") |>
    dplyr::mutate(excess_confidence = .data$synthetic - .data$holdout) |>
    dplyr::relocate(dplyr::all_of(c("target_var", "synthetic", "holdout", "excess_confidence")))

  return(result)
}
