#' @title Helper function for the k-marginals metric
#'
#' @description This helper function takes a specified k-marginal and calculates
#' all unique k-combinations of variables shared by the input data. For each
#' combination, tibbles containing unique combinations of observed value levels
#' are created with the marginal probabilities for each cell for both
#' synthetic and confidential data included. The mean absolute
#' difference (MabsDD) between the synthetic and confidential data's marginal
#' probabilities is computed for each k-combination. The MabsDDs are
#' averaged into a scalar score equal to mean(MabsDD).
#'
#' @param synth_data A tibble with synthetic data.
#' @param conf_data A tibble with confidential data.
#' @param k Scalar order of the k-marginal (valid range = 1:3).
#' @param keep_marginals Single integer number of worst marginals to retain
#' in the output.
#' Defaults to `Inf`, which retains all of them.
#' @param keep_cells Single integer number of worst cells to retain in the
#' output. Defaults to `Inf`, which retains all of them.
#' @param n_marginals Single integer cap on the number of variable
#' combinations to evaluate. When the number of possible combinations exceeds
#' the cap, a random subset is sampled; set a seed before calling for
#' reproducible results. Defaults to `Inf`, which evaluates all combinations.
#' @param priority_vars Optional character vector of variable names. Every
#' combination containing at least one of these variables is always evaluated;
#' sampling under `n_marginals` only applies to the remaining combinations. If
#' the priority combinations alone exceed `n_marginals`, all of them are still
#' evaluated. Defaults to `NULL`.
#' @param weight_var Optional character name of a numeric sample-weight
#' column present in both datasets. When set, cell proportions are weight
#' shares instead of row shares, and the weight column is excluded from the
#' marginals. Weights must be finite and non-negative with a positive total.
#' Defaults to `NULL` (unweighted).
#' @param group_by Optional character vector of grouping variable names
#' present in both datasets. When set, the grouping variables are excluded
#' from the marginals and the metric is computed within each stratum observed
#' in the confidential data; the headline score is the mean of the per-stratum
#' scores weighted by each stratum's confidential share (weight share when
#' `weight_var` is set). Defaults to `NULL` (no stratification).
#' @param synth_varnames Optional character vector of synthesized variable
#' names. When set, only these variables (intersected with the variables
#' shared by both datasets) contribute marginals. Defaults to `NULL`, which
#' places no restriction on the shared variables.
#' @param na.rm A logical for ignoring `NA` values in proportion
#' calculations. When `FALSE`, missing values form their own `"NA"` level in
#' each marginal (and a message lists the affected variables); when `TRUE`,
#' rows with a missing value are dropped from each marginal that uses the
#' affected variable, leaving marginals of complete variables untouched.
#' Defaults to `FALSE`.
#' @param bins Optional single integer >= 2. When set, every numeric shared
#' variable is discretized into this many bins (fewer, with a warning, if
#' tied quantile cut points collapse) with breaks derived from the
#' observed (non-missing) confidential values and applied to both datasets;
#' the outer bins extend to +/-Inf so synthetic values outside the
#' confidential range land in edge bins, and missing values follow `na.rm`
#' like any other variable. Defaults to `NULL` (no discretization).
#' @param discretize_method Method used to place bin breaks when `bins` is
#' set: "width" for fixed binwidths, "ntile" for quantile bins, or "cluster"
#' for univariate k-means clustering (set a seed before calling for
#' reproducible clusters). Defaults to "width".
#'
#' @return A `k_marginals` object with three elements: `score`, a value in
#' range 0 to 1 where 0 is a perfect match, 1 is the worst mismatch, and
#' larger values denote greater discrepancy between confidential and
#' synthetic data; `marginals`, a
#' tibble with the MabsDD for each combination of variables, worst first; and
#' `cells`, a tibble with the synthetic and confidential proportions and their
#' absolute difference for every cell, worst first. `score` is always computed
#' from all evaluated marginals, even when `keep_marginals` or `keep_cells`
#' truncate the detail tables. When `group_by` is set, `marginals` and
#' `cells` gain the grouping columns and a fourth element `group_scores`
#' reports each stratum's share and score, worst first.
#'
.util_k_marginals <- function(
  synth_data,
  conf_data,
  k,
  keep_marginals = Inf,
  keep_cells = Inf,
  n_marginals = Inf,
  priority_vars = NULL,
  weight_var = NULL,
  group_by = NULL,
  synth_varnames = NULL,
  na.rm = FALSE,
  bins = NULL,
  discretize_method = c("width", "ntile", "cluster")
) {
  discretize_method <- match.arg(discretize_method)

  .validate_k_marginals_helper_args(
    na.rm = na.rm,
    keep_marginals = keep_marginals,
    keep_cells = keep_cells,
    n_marginals = n_marginals,
    k = k
  )

  stopifnot(inherits(synth_data, "data.frame"))
  stopifnot(inherits(conf_data, "data.frame"))

  if (nrow(synth_data) == 0 || nrow(conf_data) == 0) {
    stop("`synth_data` and `conf_data` must each contain at least one row")
  }

  prepared_inputs <- .prepare_k_marginals_inputs(
    synth_data = synth_data,
    conf_data = conf_data,
    k = k,
    weight_var = weight_var,
    group_by = group_by,
    synth_varnames = synth_varnames,
    na.rm = na.rm,
    bins = bins,
    discretize_method = discretize_method
  )

  synth_data <- prepared_inputs$synth_data
  conf_data <- prepared_inputs$conf_data
  shared_vars <- prepared_inputs$shared_vars

  if (!is.null(priority_vars)) {
    if (!(is.character(priority_vars) &&
      all(priority_vars %in% shared_vars))) {
      stop(
        "`priority_vars` must be a character vector of variables available ",
        "for marginals after shared-variable, `group_by`, `weight_var`, ",
        "and `synth_varnames` filtering"
      )
    }
  }

  kmarginals_vars <- .select_k_marginal_combos(
    shared_vars = shared_vars,
    k = k,
    n_marginals = n_marginals,
    priority_vars = priority_vars
  )

  if (is.null(group_by)) {
    cells <- .compute_marginal_cells(
      synth_data = synth_data,
      conf_data = conf_data,
      combos = kmarginals_vars,
      weight_var = weight_var,
      na.rm = na.rm,
      allow_empty_synth = FALSE
    ) |>
      dplyr::arrange(dplyr::desc(.data$abs_diff))

    # MabsDD per combination, worst marginals first
    marginals <- cells |>
      dplyr::summarize(madd = mean(.data$abs_diff), .by = "variables") |>
      dplyr::arrange(dplyr::desc(.data$madd))

    # mean of the MabsDDs, computed from all marginals before any truncation
    result <- structure(
      list(
        score = mean(marginals$madd),
        marginals = utils::head(marginals, n = keep_marginals),
        cells = utils::head(cells, n = keep_cells)
      ),
      class = "k_marginals"
    )

    return(result)
  }

  stratified <- .stratify_k_marginals(
    synth_data = synth_data,
    conf_data = conf_data,
    combos = kmarginals_vars,
    group_by = group_by,
    weight_var = weight_var,
    na.rm = na.rm
  )

  result <- structure(
    list(
      score = stratified$score,
      marginals = utils::head(stratified$marginals, n = keep_marginals),
      cells = utils::head(stratified$cells, n = keep_cells),
      group_scores = stratified$group_scores
    ),
    class = "k_marginals"
  )

  return(result)
}

#' @title Validate internal helper arguments for the k-marginals metric
#'
#' @description Validates the scalar helper arguments that control NA
#' handling, truncation of detail tables, sampling ceiling, and the order of
#' the marginals. The function errors when any of the following conditions are
#' met:
#' 1. `na.rm` is not a logical scalar.
#' 2. `keep_marginals`, `keep_cells`, or `n_marginals` is not a single
#' integer >= 1 or `Inf`.
#' 3. `k` is not a single integer between 1 and 3.
#'
#' @param na.rm Logical scalar controlling missing-value handling.
#' @param keep_marginals Single integer >= 1 or `Inf` giving the number of
#' marginals to retain.
#' @param keep_cells Single integer >= 1 or `Inf` giving the number of cells
#' to retain.
#' @param n_marginals Single integer >= 1 or `Inf` giving the maximum number
#' of marginals to evaluate.
#' @param k Single integer giving the k-marginal order.
#'
#' @return `TRUE` if validation passes; otherwise an error is thrown.
#'
.validate_k_marginals_helper_args <- function(
  na.rm,
  keep_marginals,
  keep_cells,
  n_marginals,
  k
) {
  is_logical_na_rm <- is.logical(na.rm)
  has_single_na_rm <- length(na.rm) == 1
  has_observed_na_rm <- !is.na(na.rm)

  if (!(is_logical_na_rm && has_single_na_rm && has_observed_na_rm)) {
    stop("`na.rm` must be a single TRUE or FALSE")
  }

  keep_values <- list(
    keep_marginals = keep_marginals,
    keep_cells = keep_cells,
    n_marginals = n_marginals
  )

  for (keep in keep_values) {
    is_numeric_keep <- is.numeric(keep)
    has_single_keep <- length(keep) == 1
    has_observed_keep <- !is.na(keep)

    if (is_numeric_keep && has_single_keep && has_observed_keep) {
      keep_at_least_one <- keep >= 1
      keep_is_integer <- keep == floor(keep)
    } else {
      keep_at_least_one <- FALSE
      keep_is_integer <- FALSE
    }

    if (!(is_numeric_keep && has_single_keep && has_observed_keep &&
      keep_at_least_one && keep_is_integer)) {
      stop(
        "`keep_marginals`, `keep_cells`, and `n_marginals` must be single ",
        "integers >= 1 or Inf"
      )
    }
  }

  is_numeric_k <- is.numeric(k)
  has_single_k <- length(k) == 1
  k_in_supported_range <- k %in% 1:3

  if (!(is_numeric_k && has_single_k && k_in_supported_range)) {
    stop("`k` must be a single integer between 1 and 3")
  }

  return(TRUE)
}

#' @title Print a k_marginals object
#'
#' @param x A `k_marginals` object from [util_k_marginals()].
#' @param n Number of worst marginals to display.
#' @param ... Additional arguments passed to methods (unused).
#'
#' @return `x`, invisibly.
#'
#' @export
#'
print.k_marginals <- function(x, n = 5, ...) {
  cat("k-marginals score:", round(x$score, digits = 2), "\n\n")

  cat("Worst marginals:\n")
  print(utils::head(x$marginals, n = n))

  if (!is.null(x$group_scores)) {
    cat("\nWorst groups:\n")
    print(utils::head(x$group_scores, n = n))
  }

  return(invisible(x))
}

#' @title Calculate the k-marginals metric
#'
#' @description For each unique k-combination of variables shared by the
#' synthetic and confidential data, the mean absolute difference (MabsDD)
#' between the two datasets' marginal cell probabilities is computed. The
#' MabsDDs are averaged across combinations, with score = mean(MabsDD).
#'
#' @param eval_data An `eval_data` object.
#' @param k Scalar order of the k-marginal (valid range = 1:3).
#' @param keep_marginals Single integer number of worst marginals to retain
#' in the output.
#' Defaults to `Inf`, which retains all of them.
#' @param keep_cells Single integer number of worst cells to retain in the
#' output. Defaults to `Inf`, which retains all of them.
#' @param n_marginals Single integer cap on the number of variable
#' combinations to evaluate. When the number of possible combinations exceeds
#' the cap, a random subset is sampled; set a seed before calling for
#' reproducible results. Defaults to `Inf`, which evaluates all combinations.
#' @param priority_vars Optional character vector of variable names. Every
#' combination containing at least one of these variables is always evaluated;
#' sampling under `n_marginals` only applies to the remaining combinations. If
#' the priority combinations alone exceed `n_marginals`, all of them are still
#' evaluated. Defaults to `NULL`.
#' @param weight_var Optional character name of a numeric sample-weight
#' column present in both datasets. When set, cell proportions are weight
#' shares instead of row shares, and the weight column is excluded from the
#' marginals. Weights must be finite and non-negative with a positive total.
#' Defaults to `NULL` (unweighted).
#' @param group_by Optional character vector of grouping variable names
#' present in both datasets. When set, the grouping variables are excluded
#' from the marginals and the metric is computed within each stratum observed
#' in the confidential data; the headline score is the mean of the per-stratum
#' scores weighted by each stratum's confidential share (weight share when
#' `weight_var` is set). Defaults to `NULL` (no stratification).
#' @param synth_vars A logical for if only synthesized variables should
#' contribute marginals. Only meaningful when the `eval_data` records which
#' variables were synthesized (i.e., was built from a `postsynth`); for plain
#' data frames all shared variables are used regardless. Defaults to `TRUE`.
#' @param na.rm A logical for ignoring `NA` values in proportion
#' calculations. When `FALSE`, missing values form their own `"NA"` level in
#' each marginal (and a message lists the affected variables); when `TRUE`,
#' rows with a missing value are dropped from each marginal that uses the
#' affected variable, leaving marginals of complete variables untouched.
#' Defaults to `FALSE`.
#' @param bins Optional single integer >= 2. When set, every numeric shared
#' variable is discretized into this many bins (fewer, with a warning, if
#' tied quantile cut points collapse) with breaks derived from the
#' observed (non-missing) confidential values and applied to both datasets;
#' the outer bins extend to +/-Inf so synthetic values outside the
#' confidential range land in edge bins, and missing values follow `na.rm`
#' like any other variable. Defaults to `NULL` (no discretization).
#' @param discretize_method Method used to place bin breaks when `bins` is
#' set: "width" for fixed binwidths, "ntile" for quantile bins, or "cluster"
#' for univariate k-means clustering (set a seed before calling for
#' reproducible clusters). Defaults to "width".
#'
#' @return A `k_marginals` object with three elements: `score`, a value in
#' range 0 to 1 where 0 is a perfect match, 1 is the worst mismatch, and
#' larger values denote greater discrepancy between confidential and
#' synthetic data; `marginals`, a
#' tibble with the MabsDD for each combination of variables, worst first; and
#' `cells`, a tibble with the synthetic and confidential proportions and their
#' absolute difference for every cell, worst first. `score` is always computed
#' from all evaluated marginals, even when `keep_marginals` or `keep_cells`
#' truncate the detail tables. When `group_by` is set, `marginals` and
#' `cells` gain the grouping columns and a fourth element `group_scores`
#' reports each stratum's share and score, worst first. For multiple replicates, a list of such
#' objects, one per replicate.
#'
#' @export
#'
util_k_marginals <- function(
  eval_data,
  k = 1,
  keep_marginals = Inf,
  keep_cells = Inf,
  n_marginals = Inf,
  priority_vars = NULL,
  weight_var = NULL,
  group_by = NULL,
  synth_vars = TRUE,
  na.rm = FALSE,
  bins = NULL,
  discretize_method = c("width", "ntile", "cluster")
) {
  stopifnot(is_eval_data(eval_data))

  discretize_method <- match.arg(discretize_method)

  if (!(is.logical(synth_vars) && length(synth_vars) == 1 &&
    !is.na(synth_vars))) {
    stop("`synth_vars` must be a single TRUE or FALSE")
  }

  # NULL for plain data frame eval_data, so the helper applies no restriction
  synth_varnames <- if (synth_vars) {
    eval_data$synth_vars
  } else {
    NULL
  }

  # empty metadata can only come from a user-supplied eval_data(synth_vars =)
  # argument; fail with a message in terms of this function's arguments
  if (!is.null(synth_varnames) && length(synth_varnames) == 0) {
    stop(
      "`eval_data` records no synthesized variables; use `synth_vars = ",
      "FALSE` to evaluate all shared variables"
    )
  }

  # surface the resolved method so a forgotten discretize_method is visible
  if (!is.null(bins)) {
    message(
      "Discretizing numeric variables into ", bins,
      " bins using the '", discretize_method, "' method"
    )
  }

  if (eval_data$n_rep == 1) {
    return(
      .util_k_marginals(
        synth_data = eval_data$synth_data,
        conf_data = eval_data$conf_data,
        k = k,
        keep_marginals = keep_marginals,
        keep_cells = keep_cells,
        n_marginals = n_marginals,
        priority_vars = priority_vars,
        weight_var = weight_var,
        group_by = group_by,
        synth_varnames = synth_varnames,
        na.rm = na.rm,
        bins = bins,
        discretize_method = discretize_method
      )
    )
  } else {
    result <- purrr::map(
      .x = eval_data$synth_data,
      .f = \(sd) {
        .util_k_marginals(
          synth_data = sd,
          conf_data = eval_data$conf_data,
          k = k,
          keep_marginals = keep_marginals,
          keep_cells = keep_cells,
          n_marginals = n_marginals,
          priority_vars = priority_vars,
          weight_var = weight_var,
          group_by = group_by,
          synth_varnames = synth_varnames,
          na.rm = na.rm,
          bins = bins,
          discretize_method = discretize_method
        )
      }
    )

    return(result)
  }
}
