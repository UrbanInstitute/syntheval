#'
#' Excess predictability: increase in predictability attributable to the synthetic data
#'
#' @param attribute_scan An `attribute_scan` object. Must include holdout data
#' (i.e. `attribute_scan$holdout` is not `NULL`).
#'
#' @returns A tibble with columns `target_var`, `synthetic` and `holdout` (the
#' per-target `scan_predictability()` values for each source), and
#' `excess_predictability` (`synthetic` minus `holdout`). Values above 0
#' indicate the synthetic data is more predictable of the confidential target
#' than the holdout data, suggesting higher attribute inference risk
#' attributable to the synthetic data itself rather than superpopulation
#' characteristics.
#'
#' @export
#'
scan_excess_predictability <- function(attribute_scan) {
  stopifnot(is_attribute_scan(attribute_scan))

  if (is.null(attribute_scan$holdout)) {
    stop(
      "Error: attribute_scan must include holdout data to calculate ",
      "scan_excess_predictability(). Supply holdout_data to eval_data()."
    )
  }

  predictability <- scan_predictability(attribute_scan)

  result <- predictability |>
    tidyr::pivot_wider(names_from = "source", values_from = "predictability") |>
    dplyr::mutate(excess_predictability = .data$synthetic - .data$holdout) |>
    dplyr::relocate(dplyr::all_of(c("target_var", "synthetic", "holdout", "excess_predictability")))

  return(result)
}
