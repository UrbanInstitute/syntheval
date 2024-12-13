#' Regression confidence interval overlap
#'
#' @param postsynth A postsynth object or tibble with synthetic data
#' @param data A data frame with the original data
#' @param formula A formula for a linear regression model
#'
#' @return A list of two dataframes:
#'   * `ci_overlap`: one row per model parameter with utility metrics.
#'     * `overlap `: symmetric overlap metric, calculated as the average of the 
#'       interval overlap contained in the synthetic confidence interval and the 
#'       interval overlap contained in the confidential confidence interval.
#'     * `coef_diff`: synthetic parameter estimate - confidential parameter estimate
#'     * `std_coef_diff`: `coef_diff` divided by the standard error for the confidential data.
#'     * `sign_match`: boolean if the synthetic and confidential parameter estimates have the same sign.
#'     * `significance_match`: boolean if the null hypothesis test where the 
#'       parameter is 0 has p-value less than .05 agrees in both confidential and
#'       synthetic data.
#'     * `ss`: boolean if both `sign_match` and `significance_match` are true.
#'     * `sso`: boolean if `sign_match` is true and `overlap` is positive. 
#'   * `coef_diff`: one row per model parameter and data source (confidential or 
#'     synthetic) listing parameter estimates, standard errors, test statistics,
#'     p-values for null hypothesis tests, and 95% confidence interval bounds.
#' 
#' @examples
#' conf_data <- mtcars
#' synth_data <- mtcars %>% 
#'    dplyr::slice_sample(n = nrow(mtcars) / 2)
#' 
#' util_ci_overlap(
#'   conf_data,
#'   synth_data,
#'   mpg ~ disp + vs + am
#' )
#' 
#' @family Utility metrics
#' 
#' @export
util_ci_overlap <- function(postsynth, data, formula) {
  
  if (is_postsynth(postsynth)) {
    
    synthetic_data <- postsynth$synthetic_data
    
  } else {
    
    synthetic_data <- postsynth
    
  }
  
  # original model ------------------------------------------------------
  lm_original <- stats::lm(formula = formula, data = data)
  
  # synthetic model ---------------------------------------------------------
  lm_synth <- stats::lm(formula = formula, data = synthetic_data)
  
  coefficients <- dplyr::bind_rows(
    `original` = broom::tidy(lm_original, conf.int = TRUE),
    synthetic = broom::tidy(lm_synth, conf.int = TRUE),
    .id = "source"
  )
  
  diff_table <- dplyr::full_join(
    broom::tidy(lm_original, conf.int = TRUE),
    broom::tidy(lm_synth, conf.int = TRUE),
    by = "term",
    suffix = c("_original", "_synthetic")
  )
  
  ci_overlap <- diff_table %>%
    # calculate max bounds for formula
    dplyr::mutate(
      overlap_lower = pmax(.data$conf.low_original, .data$conf.low_synthetic),
      overlap_upper = pmin(.data$conf.high_original, .data$conf.high_synthetic)
    ) %>%
    # calculate confidence interval overlap
    dplyr::mutate(
      overlap = 0.5 * (((.data$overlap_upper - .data$overlap_lower) / (.data$conf.high_original - .data$conf.low_original)) +
                         ((.data$overlap_upper - .data$overlap_lower) / (.data$conf.high_synthetic - .data$conf.low_synthetic)))
    ) %>%
    # calculate other regression metrics
    dplyr::mutate(
      coef_diff = .data$estimate_synthetic - .data$estimate_original,
      std_coef_diff = (.data$estimate_synthetic - .data$estimate_original) / .data$std.error_original,
      sign_match = (.data$estimate_original <= 0 & .data$estimate_synthetic <= 0) | 
        (.data$estimate_original > 0 & .data$estimate_synthetic > 0),
      significance_match = (.data$p.value_original < 0.05 & .data$p.value_synthetic < 0.05) | 
        (.data$p.value_original > 0.05 & .data$p.value_synthetic > 0.05),
      ss_match = .data$sign_match & .data$significance_match,
      sso_match = .data$sign_match & .data$overlap > 0
    ) %>%
    dplyr::select(
      "term", "overlap", "coef_diff", "std_coef_diff", "sign_match", 
      "significance_match", "ss_match", "sso_match"
    )
  
  
  list(
    ci_overlap = ci_overlap,
    coefficient = coefficients
  )
  
}
