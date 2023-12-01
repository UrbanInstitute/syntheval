#' Regression confidence interval overlap
#'
#' @param postsynth A postsynth object or tibble with synthetic data
#' @param data A data frame with the original data
#' @param formula A formula for a linear regression model
#'
#' @return A list with the regression confidence interval overlap and estimated 
#' coefficients
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