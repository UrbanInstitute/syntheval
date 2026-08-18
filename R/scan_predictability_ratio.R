#'
#' Predictability ratio: synthetic-data predictability relative to holdout-data predictability
#'
#' @param attribute_scan An `attribute_scan` object. Must include holdout data
#' (i.e. `attribute_scan$holdout` is not `NULL`).
#'
#' @returns A tibble with columns `target_var`, `synthetic` and `holdout`
#' (the per-target `scan_predictability()` values for each source), and
#' `predictability_ratio` (`synthetic` divided by `holdout`). Values above 1
#' indicate the synthetic data is more predictable of the confidential target
#' than the holdout data, suggesting higher attribute inference risk.
#'
#' @export
#'
scan_predictability_ratio <- function(attribute_scan) {
  stopifnot(is_attribute_scan(attribute_scan))

  if (is.null(attribute_scan$holdout)) {
    stop(
      "Error: attribute_scan must include holdout data to calculate ",
      "scan_predictability_ratio(). Supply holdout_data to eval_data()."
    )
  }

  predictability <- scan_predictability(attribute_scan)

  result <- predictability |>
    tidyr::pivot_wider(names_from = "source", values_from = "predictability") |>
    dplyr::mutate(predictability_ratio = .data$synthetic / .data$holdout) |>
    dplyr::relocate(dplyr::all_of(c("target_var", "synthetic", "holdout", "predictability_ratio")))

  return(result)
}
