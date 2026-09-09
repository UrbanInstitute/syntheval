#' Regression confidence interval overlap for one synthetic data replicate
#'
#' @param synth_data A data.frame with synthetic data
#' @param conf_data A data.frame with the confidential data
#' @param formula A formula for a linear regression model
#' @param family An optional family argument, exactly as accepted by 
#'   `stats::glm()`. When `NULL` (the default), `stats::lm()` is used to fit
#'   the model.
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
.util_ci_overlap <- function(synth_data, conf_data, formula, family = NULL) {
  
  
  # original model ------------------------------------------------------
  if (is.null(family)) {
    lm_original <- stats::lm(formula = formula, data = conf_data)
  } else {
    lm_original <- stats::glm(formula = formula, data = conf_data, family = family)
  }
  
  # synthetic model ---------------------------------------------------------
  if (is.null(family)) {
    lm_synth <- stats::lm(formula = formula, data = synth_data)
  } else {
    lm_synth <- stats::glm(formula = formula, data = synth_data, family = family)
  }
  
  compute_ci_overlap(original_model = lm_original, synthetic_model = lm_synth)
  
}

#' Regression confidence interval overlap for fitted models
#'
#' For most use cases, use [util_ci_overlap()] instead, which fits the models
#' for you from an `eval_data` object and a formula. Use `compute_ci_overlap()`
#' directly only when you already have your own fitted models to compare.
#'
#' @param original_model A model fit on the confidential data, supported by 
#'   `broom::tidy()` (e.g. an `lm` or `glm` object)
#' @param synthetic_model A model of the same form as `original_model`, fit on
#'   the synthetic data
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
#' @family Utility metrics
#' 
#' @examples
#' conf_data <- mtcars
#' synth_data <- mtcars %>% 
#'    dplyr::slice_sample(n = nrow(mtcars) / 2)
#' 
#' original_model <- lm(mpg ~ disp + vs + am, data = conf_data)
#' synthetic_model <- lm(mpg ~ disp + vs + am, data = synth_data)
#' 
#' compute_ci_overlap(original_model, synthetic_model)
#' 
#' @export
#' 
compute_ci_overlap <- function(original_model, synthetic_model) {
  
  coefficients <- dplyr::bind_rows(
    `original` = broom::tidy(original_model, conf.int = TRUE),
    synthetic = broom::tidy(synthetic_model, conf.int = TRUE),
    .id = "source"
  )
  
  diff_table <- dplyr::full_join(
    broom::tidy(original_model, conf.int = TRUE),
    broom::tidy(synthetic_model, conf.int = TRUE),
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

#' Regression confidence interval overlap
#'
#' This is the main function for computing confidence interval overlap: it
#' fits models on the confidential and synthetic data and compares them. If
#' you already have fitted models to compare, use [compute_ci_overlap()]
#' instead.
#'
#' @param eval_data An `eval_data` object
#' @param formula A formula for a linear regression model
#' @param family An optional family argument, exactly as accepted by 
#'   `stats::glm()`. When `NULL` (the default), `stats::lm()` is used to fit
#'   the model.
#'
#' @return A list of two dataframes (one per each synthetic data replicate):
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
#' @family Utility metrics
#' 
#' @examples
#' conf_data <- mtcars
#' synth_data <- mtcars %>% 
#'    dplyr::slice_sample(n = nrow(mtcars) / 2)
#'  
#' eval_data <- eval_data(conf_data, synth_data)
#' 
#' util_ci_overlap(
#'   eval_data,
#'   mpg ~ disp + vs + am
#' )
#' 
#' util_ci_overlap(
#'   eval_data,
#'   am ~ disp + vs,
#'   family = binomial
#' )
#' 
#' @export
#' 
util_ci_overlap <- function(eval_data, formula, family = NULL) {
  
  stopifnot(is_eval_data(eval_data))
  
  if (eval_data$n_rep == 1) {
    
    return(
      .util_ci_overlap(
        conf_data = eval_data$conf_data, 
        synth_data = eval_data$synth_data, 
        formula = formula,
        family = family
      )
    )
      
  } else {
    
    result <- purrr::map(
      .x = eval_data$synth_data,
      .f = \(sd) {
        
        .util_ci_overlap(
          conf_data = eval_data$conf_data, 
          synth_data = sd, 
          formula = formula,
          family = family
        )
        
      }
    )
    
    return(result)
    
  }
  
}
