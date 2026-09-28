#'
#' Test how missingness in each variable relates to every other variable
#'
#' @param conf_data A data frame with the confidential data
#' @param synth_data A data frame with the synthetic data
#' @param holdout_data An optional data frame with the holdout data
#' @param na_values A character vector of values that should be treated as
#' missing in addition to `NA`
#' @param alpha A significance threshold applied to BH-adjusted p-values when
#' classifying pairs in `na_relate_summary`. Defaults to `0.05`.
#'
#' @return A `list` with `na_relate` and `na_relate_summary` tibbles, whose
#' columns are described in [util_na_relate()]
#'
.util_na_relate <- function(
    conf_data,
    synth_data,
    holdout_data = NULL,
    na_values = NULL,
    alpha = 0.05
) {

  # recode custom NA values in the data frames
  conf_data <- .recode_custom_na(conf_data, na_values = na_values)
  synth_data <- .recode_custom_na(synth_data, na_values = na_values)

  if (!is.null(holdout_data)) {
    holdout_data <- .recode_custom_na(holdout_data, na_values = na_values)
  }

  # a variable missing from conf or synth can't be compared across the two
  common_vars <- intersect(names(conf_data), names(synth_data))

  # na_vars are variables with any missingness in conf or synth
  has_na_lgl <- purrr::map_lgl(conf_data[common_vars], ~ any(is.na(.x))) |
    purrr::map_lgl(synth_data[common_vars], ~ any(is.na(.x)))

  na_vars <- common_vars[has_na_lgl]

  if (length(na_vars) == 0) {
    stop("ERROR: no variables with missing values are present")
  }

  # create all na_var x related_var pairs, excluding self-pairs
  pairs <- tidyr::expand_grid(
    na_var = na_vars,
    related_var = common_vars
  ) |>
    dplyr::filter(.data$na_var != .data$related_var)

  # fit_row(): run the confidential/synthetic (/holdout) missingness
  # association tests for one na_var x related_var pair, returning a one-row
  # tibble with method, effect_size_*, and p_value_* columns for confidential,
  # synthetic (and holdout when holdout_data is supplied)
  fit_row <- function(na_var, related_var) {

    # a categorical vs. numeric related_var is fixed by the confidential data
    method <- if (is.numeric(conf_data[[related_var]])) "t_test" else "fisher"

    confidential <- .na_indicator_test(
      data = conf_data,
      na_var = na_var,
      related_var = related_var,
      method = method
    )

    synthetic <- .na_indicator_test(
      data = synth_data,
      na_var = na_var,
      related_var = related_var,
      method = method
    )

    row <- tibble::tibble(
      na_var = na_var,
      related_var = related_var,
      method = method,
      effect_size_confidential = confidential$effect_size,
      effect_size_synthetic = synthetic$effect_size,
      effect_size_difference = synthetic$effect_size - confidential$effect_size,
      p_value_confidential = confidential$p_value,
      p_value_synthetic = synthetic$p_value
    )

    if (!is.null(holdout_data)) {

      holdout <- .na_indicator_test(
        data = holdout_data,
        na_var = na_var,
        related_var = related_var,
        method = method
      )

      row <- row |>
        dplyr::mutate(
          effect_size_holdout = holdout$effect_size,
          p_value_holdout = holdout$p_value
        )

    }

    return(row)

  }

  na_relate <- purrr::map2(
    .x = pairs$na_var,
    .y = pairs$related_var,
    .f = fit_row
  ) |>
    dplyr::bind_rows()

  # BH-adjust within each source to correct for multiple comparisons
  na_relate <- na_relate |>
    dplyr::mutate(
      p_value_adj_confidential = stats::p.adjust(
        .data$p_value_confidential,
        method = "BH"
      ),
      p_value_adj_synthetic = stats::p.adjust(
        .data$p_value_synthetic,
        method = "BH"
      )
    )

  if (!is.null(holdout_data)) {
    na_relate <- na_relate |>
      dplyr::mutate(
        p_value_adj_holdout = stats::p.adjust(
          .data$p_value_holdout,
          method = "BH"
        )
      )
  }

  # label each pair by whether it is significant in confidential and synthetic
  na_relate <- na_relate |>
    dplyr::mutate(
      sig_confidential = !is.na(.data$p_value_adj_confidential) &
        .data$p_value_adj_confidential < alpha,
      sig_synthetic = !is.na(.data$p_value_adj_synthetic) &
        .data$p_value_adj_synthetic < alpha,
      label = dplyr::case_when(
        .data$sig_confidential & .data$sig_synthetic ~ "preserved",
        .data$sig_confidential & !.data$sig_synthetic ~ "lost",
        !.data$sig_confidential & .data$sig_synthetic ~ "spurious",
        TRUE ~ "consistent_null"
      )
    )

  # summarize each na_var, plus an "overall" row across all na_vars,
  # by duplicating the rows with na_var relabeled before one group_by()
  na_relate_summary <- dplyr::bind_rows(
    na_relate,
    dplyr::mutate(na_relate, na_var = "overall")
  ) |>
    dplyr::group_by(.data$na_var) |>
    dplyr::summarise(
      n_pairs = dplyr::n(),
      preserved = sum(.data$label == "preserved"),
      lost = sum(.data$label == "lost"),
      spurious = sum(.data$label == "spurious"),
      consistent_null = sum(.data$label == "consistent_null"),
      # count of relationships with detected effects
      n_effect_size = sum(
        .data$sig_confidential & !is.na(.data$effect_size_difference)
      ),
      # restricted to observations with detected effects
      effect_size_mae = mean(
        abs(.data$effect_size_difference[.data$sig_confidential]),
        na.rm = TRUE
      ),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      # mean() of an empty selection returns NaN
      effect_size_mae = dplyr::if_else(
        is.nan(.data$effect_size_mae),
        NA_real_,
        .data$effect_size_mae
      ),
      concordance_rate = dplyr::if_else(
        .data$preserved + .data$lost > 0,
        .data$preserved / (.data$preserved + .data$lost),
        NA_real_
      )
    ) |>
    dplyr::arrange(.data$na_var == "overall", .data$na_var)

  na_relate <- na_relate |>
    dplyr::select(
      -"effect_size_difference",
      -"p_value_confidential", -"p_value_synthetic",
      -dplyr::any_of("p_value_holdout"),
      -"sig_confidential", -"sig_synthetic", -"label"
    )

  return(
    list(
      na_relate = na_relate,
      na_relate_summary = na_relate_summary
    )
  )

}

#'
#' Run a t-test or Fisher's exact test between a missingness indicator and a
#' related variable
#'
#' @param data A data frame
#' @param na_var The name of the variable with missingness
#' @param related_var The name of the variable to test association against
#' @param method `"t_test"` or `"fisher"`
#'
#' @return A `list` with `effect_size` and `p_value`, both `NA` if the pair
#' cannot be fit (degenerate indicator/related variable, or a model-fit error)
#'
.na_indicator_test <- function(data, na_var, related_var, method) {

  no_result <- list(effect_size = NA_real_, p_value = NA_real_)

  # return the empty table if the holdout data have no variables common with
  # the synthetic and confidential data
  if (!all(c(na_var, related_var) %in% names(data))) {
    return(no_result)
  }

  na_indicator <- is.na(data[[na_var]])
  related <- data[[related_var]]

  # can't fit if the na_indicator or related variable has no variation
  has_variation <- length(unique(na_indicator)) >= 2 &&
    length(unique(stats::na.omit(related))) >= 2

  if (!has_variation) {
    return(no_result)
  }

  # run the appropriate test based on the specified method
  # wrap the test in tryCatch to handle potential errors gracefully
  result <- tryCatch({

    if (method == "t_test") {

      # Welch's t-test for the p-value, pooled-SD Cohen's d for the effect size
      fit <- stats::t.test(related ~ na_indicator)

      grp_na <- stats::na.omit(related[na_indicator])
      grp_no_na <- stats::na.omit(related[!na_indicator])

      # calculate the pooled standard deviation for the two groups
      s2_1 <- stats::var(grp_na)
      n1 <- length(grp_na)
      s2_2 <- stats::var(grp_no_na)
      n2 <- length(grp_no_na)
      
      pooled_sd <- sqrt(
        ((n1 - 1) * s2_1 + (n2 - 1) * s2_2) / (n1 + n2 - 2)
      )

      effect_size <- (mean(grp_na) - mean(grp_no_na)) / pooled_sd

    } else {

      # Fisher's exact test avoids the (quasi-)separation glm() runs into
      # when missingness is common to one level of a categorical related_var
      tab <- table(na_indicator, related)

      # the exact computation exhausts its workspace on larger tables
      fit <- tryCatch(
        stats::fisher.test(tab),
        error = function(e) stats::fisher.test(tab, simulate.p.value = TRUE)
      )

      # an odds ratio estimate is only available for 2x2 tables
      log_or <- if (all(dim(tab) == 2)) unname(log(fit$estimate)) else NA_real_

      # perfect separation gives an infinite odds ratio
      effect_size <- if (is.finite(log_or)) log_or else NA_real_

    }

    list(effect_size = effect_size, p_value = fit$p.value)

  }, error = function(e) {

    message(sprintf(
      "missingness test (%s) failed for %s ~ %s: %s",
      method, na_var, related_var, conditionMessage(e)
    ))

    no_result

  })

  return(result)

}

#'
#' Test how missingness in each variable relates to every other variable
#'
#' @param eval_data An `eval_data` object
#' @param na_values A character vector of values that should be treated as
#' missing in addition to `NA`
#' @param alpha A significance threshold applied to BH-adjusted p-values when
#' classifying pairs in `na_relate_summary`. Defaults to `0.05`.
#'
#' @return A `list` (one per synthetic data replicate) with two tibbles,
#' restricted to the variables common to the confidential and synthetic data:
#'  - `na_relate`: one row per variable with a missing value (`na_var`) by
#'    related variable (`related_var`), with diagnostics about the bivariate
#'    relationship between the missingness and the related variable.
#'    - `method`: `"t_test"` when `related_var` is numeric, `"fisher"` when
#'      categorical.
#'    - `effect_size_confidential`, `effect_size_synthetic`: Cohen's d for
#'      `"t_test"` and the log odds ratio for `"fisher"` (`NA` when the table
#'      is larger than 2x2 or the odds ratio is not finite). A positive value
#'      means `related_var` is larger, or its second level is more common,
#'      when `na_var` is missing.
#'    - `p_value_adj_confidential`, `p_value_adj_synthetic`: BH-adjusted
#'      p-values. The false discovery rate is only controlled within a single
#'      call, so adjusted p-values from separate calls (e.g. after subsetting
#'      `na_var`s) are not comparable.
#'    - `effect_size_holdout`, `p_value_adj_holdout`: the same quantities for
#'      the holdout data when supplied, and `NA` for pairs whose variables are
#'      absent from it.
#'  - `na_relate_summary`: one row per `na_var` plus an overall row.
#'    - counts of pairs by where the relationship is significant: `preserved`
#'      (confidential and synthetic), `lost` (confidential only), `spurious`
#'      (synthetic only), and `consistent_null` (neither).
#'    - `effect_size_mae`: covers only pairs significant in the confidential
#'      data that have an effect size in both sources, so it mixes Cohen's d
#'      and log odds ratios and is not on a single scale.
#'    - `n_effect_size`: the number of relationships included in
#'      `effect_size_mae`.
#'    - `concordance_rate`: `preserved / (preserved + lost)`.
#'
#' @family utility metrics
#'
#' @examples
#' ed <- eval_data(conf_data = acs_conf, synth_data = acs_lr_synths[[1]])
#'
#' util_na_relate(ed)
#'
#' @export
#'
util_na_relate <- function(eval_data, na_values = NULL, alpha = 0.05) {

  stopifnot(is_eval_data(eval_data))

  if (eval_data$n_rep == 1) {

    result <- .util_na_relate(
      conf_data = eval_data$conf_data,
      synth_data = eval_data$synth_data,
      holdout_data = eval_data$holdout_data,
      na_values = na_values,
      alpha = alpha
    )

    return(result)

  } else {

    result <- purrr::map(
      .x = eval_data$synth_data,
      .f = \(sd) {

        .util_na_relate(
          conf_data = eval_data$conf_data,
          synth_data = sd,
          holdout_data = eval_data$holdout_data,
          na_values = na_values,
          alpha = alpha
        )

      }
    )

    return(result)

  }

}
