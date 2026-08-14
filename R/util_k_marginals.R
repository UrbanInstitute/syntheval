#' @title Worker function for the k-marginals metric
#'
#' @description This worker function takes a specified k-marginal and calculates
#' all unique k-combinations of variables shared by the input data. For each
#' combination, tibbles containing unique combinations of observed value levels
#' are created with the marginal probabilities for each cell for both
#' synthetic and confidential data included. The mean absolute
#' difference (MabsDD) between the synthetic and confidential data's marginal
#' probabilities is computed for each k-combination. The MabsDDs are
#' averaged into a scalar before being rescaled (1 - mean) * 1000.
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
#' range [0, 1000] where a higher value denotes lower MabsDDs and consequently
#' greater similarity between confidential and synthetic data; `marginals`, a
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

  if (!(is.logical(na.rm) && length(na.rm) == 1 && !is.na(na.rm))) {
    stop("`na.rm` must be a single TRUE or FALSE")
  }

  for (keep in list(keep_marginals, keep_cells, n_marginals)) {
    if (!(is.numeric(keep) && length(keep) == 1 && !is.na(keep) &&
      keep >= 1 && keep == floor(keep))) {
      stop(
        "`keep_marginals`, `keep_cells`, and `n_marginals` must be single ",
        "integers >= 1 or Inf"
      )
    }
  }

  if (!(is.numeric(k) && length(k) == 1 && k %in% 1:3)) {
    stop("`k` must be a single integer between 1 and 3")
  }

  stopifnot(inherits(synth_data, "data.frame"))
  stopifnot(inherits(conf_data, "data.frame"))

  if (nrow(synth_data) == 0 || nrow(conf_data) == 0) {
    stop("`synth_data` and `conf_data` must each contain at least one row")
  }

  if (!is.null(weight_var)) {
    if (!(is.character(weight_var) && length(weight_var) == 1)) {
      stop("`weight_var` must be a single character string")
    }

    if (!(weight_var %in% names(synth_data) &&
      weight_var %in% names(conf_data))) {
      stop("`weight_var` must be a column in both datasets")
    }

    if (!(is.numeric(synth_data[[weight_var]]) &&
      is.numeric(conf_data[[weight_var]]))) {
      stop("`weight_var` must be a numeric column in both datasets")
    }

    # invalid weights break the probability interpretation of proportions
    for (weights in list(synth_data[[weight_var]], conf_data[[weight_var]])) {
      if (!all(is.finite(weights)) || any(weights < 0) ||
        sum(weights) <= 0) {
        stop(
          "`weight_var` values must be finite and non-negative with a ",
          "positive total in both datasets"
        )
      }
    }
  }

  if (!is.null(group_by)) {
    if (!(is.character(group_by) && length(group_by) >= 1 &&
      !anyNA(group_by) &&
      all(group_by %in% names(synth_data)) &&
      all(group_by %in% names(conf_data)))) {
      stop(
        "`group_by` must be a character vector of variables present in ",
        "both datasets"
      )
    }

    if (anyDuplicated(group_by) > 0) {
      stop("`group_by` must not contain duplicate variable names")
    }

    if (!is.null(weight_var) && weight_var %in% group_by) {
      stop("`group_by` cannot include `weight_var`")
    }
  }

  # only variables present in both datasets contribute marginals; the weight
  # column and grouping variables are never themselves marginals
  shared_vars <- setdiff(
    intersect(names(synth_data), names(conf_data)),
    c(weight_var, group_by)
  )

  if (!is.null(synth_varnames)) {
    if (!(is.character(synth_varnames) && length(synth_varnames) >= 1 &&
      !anyNA(synth_varnames))) {
      stop(
        "`synth_varnames` must be a non-empty character vector without ",
        "missing values, or NULL"
      )
    }

    shared_vars <- intersect(shared_vars, synth_varnames)

    # fail here rather than at the later k check, whose message would point
    # away from the real problem
    if (length(shared_vars) == 0) {
      stop(
        "`synth_varnames` matches no variables available for marginals ",
        "after shared-variable, `group_by`, and `weight_var` filtering"
      )
    }
  }

  if (!is.null(bins)) {
    if (!(is.numeric(bins) && length(bins) == 1 && !is.na(bins) &&
      bins >= 2 && bins == floor(bins))) {
      stop("`bins` must be a single integer >= 2")
    }

    numeric_vars <- shared_vars[
      purrr::map_lgl(.x = shared_vars, .f = \(v) is.numeric(conf_data[[v]]))
    ]

    for (var in numeric_vars) {
      # cut points derive from observed values; missing values follow na.rm
      # like every other variable, becoming an NA bin or dropped rows
      conf_values <- conf_data[[var]][!is.na(conf_data[[var]])]

      if (!all(is.finite(conf_values)) || length(conf_values) == 0) {
        stop(
          "observed numeric values must be finite to discretize; `", var,
          "` is not"
        )
      }

      if (dplyr::n_distinct(conf_values) < bins) {
        stop(
          "`", var, "` has fewer distinct confidential values than `bins`"
        )
      }

      # interior cut points always derive from the confidential data; each
      # method yields bins - 1 of them
      cut_points <- switch(
        EXPR = discretize_method,
        width = seq(
          from = min(conf_values),
          to = max(conf_values),
          length.out = bins + 1
        )[2:bins],
        ntile = stats::quantile(
          x = conf_values,
          probs = seq(from = 0, to = 1, length.out = bins + 1),
          names = FALSE
        )[2:bins],
        cluster = {
          centers <- sort(
            stats::kmeans(x = conf_values, centers = bins)$centers[, 1]
          )

          (centers[-1] + centers[-length(centers)]) / 2
        }
      )

      # outer bins extend to +/-Inf so out-of-range synthetic values land in
      # edge bins; unique() collapses ties from skewed quantiles
      breaks <- unique(c(-Inf, cut_points, Inf))

      if (length(breaks) - 1 < bins) {
        warning(
          "`", var, "` was discretized into ", length(breaks) - 1,
          " bins instead of ", bins, " because of tied cut points"
        )
      }

      synth_data[[var]] <- cut(x = synth_data[[var]], breaks = breaks)
      conf_data[[var]] <- cut(x = conf_data[[var]], breaks = breaks)
    }
  }

  if (length(shared_vars) < k) {
    stop(
      "`k` cannot exceed the number of variables available for marginals ",
      "after shared-variable, `group_by`, `weight_var`, and ",
      "`synth_varnames` filtering"
    )
  }

  # grouping variables get the same NA treatment as marginal variables: an
  # "NA" stratum by default, or their incomplete rows dropped entirely
  na_vars_scope <- c(shared_vars, group_by)

  if (!na.rm) {
    na_vars <- na_vars_scope[
      purrr::map_lgl(
        .x = na_vars_scope,
        .f = \(v) anyNA(synth_data[[v]]) || anyNA(conf_data[[v]])
      )
    ]

    if (length(na_vars) > 0) {
      message(
        "Some variables contain missing data: ",
        paste(na_vars, collapse = ", ")
      )
    }

    # missing values become their own "NA" level so they participate in
    # marginals; numeric variables without bins keep NA, which count() still
    # groups separately
    synth_data[na_vars_scope] <- convert_na_to_level(synth_data[na_vars_scope])
    conf_data[na_vars_scope] <- convert_na_to_level(conf_data[na_vars_scope])
  } else if (!is.null(group_by)) {
    # rows without a stratum cannot enter any stratified marginal
    synth_data <- dplyr::filter(
      synth_data,
      !dplyr::if_any(.cols = dplyr::all_of(group_by), .fns = is.na)
    )

    conf_data <- dplyr::filter(
      conf_data,
      !dplyr::if_any(.cols = dplyr::all_of(group_by), .fns = is.na)
    )

    if (nrow(conf_data) == 0) {
      stop(
        "no confidential rows remain after removing missing `group_by` values"
      )
    }
  }

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

  kmarginals_vars <- t(utils::combn(x = shared_vars, m = k))

  # sample combinations down to n_marginals, always keeping combinations that
  # contain a priority variable
  if (nrow(kmarginals_vars) > n_marginals) {
    is_priority <- apply(
      X = kmarginals_vars,
      MARGIN = 1,
      FUN = \(vars) any(vars %in% priority_vars)
    )

    n_sampled <- min(max(n_marginals - sum(is_priority), 0), sum(!is_priority))

    sampled_rows <- sample(x = which(!is_priority), size = n_sampled)

    kmarginals_vars <- kmarginals_vars[
      sort(c(which(is_priority), sampled_rows)), ,
      drop = FALSE
    ]
  }

  # cell proportions for one dataset over one set of variables; weighted
  # proportions are weight shares instead of row shares
  process_data <- function(data, vars, prop_name, allow_empty) {
    if (na.rm) {
      data <- dplyr::filter(
        data,
        !dplyr::if_any(.cols = dplyr::all_of(vars), .fns = is.na)
      )

      if (nrow(data) == 0 && !allow_empty) {
        stop(
          "no rows remain for the marginal over ",
          paste(vars, collapse = ", "),
          " after removing missing values"
        )
      }
    }

    if (is.null(weight_var)) {
      counts <- dplyr::count(data, dplyr::across(dplyr::all_of(vars)))
    } else {
      counts <- dplyr::count(
        data,
        dplyr::across(dplyr::all_of(vars)),
        wt = .data[[weight_var]]
      )
    }

    props <- counts |>
      dplyr::mutate("{prop_name}" := .data$n / sum(.data$n)) |>
      dplyr::select(-"n")

    return(props)
  }

  # per-cell differences for one set of variables; cells absent from one
  # dataset count as 0
  marginal_cells <- function(vars, synth_d, conf_d, allow_empty_synth) {
    # only the synthetic side may be empty (a stratum the synthesis never
    # produced); a confidential marginal with no rows has nothing to score
    # against and errors inside process_data
    cells <- dplyr::full_join(
      process_data(
        data = synth_d, vars = vars, prop_name = "prop_synth",
        allow_empty = allow_empty_synth
      ),
      process_data(
        data = conf_d, vars = vars, prop_name = "prop_conf",
        allow_empty = FALSE
      ),
      by = vars
    ) |>
      tidyr::replace_na(replace = list(prop_synth = 0, prop_conf = 0)) |>
      tidyr::unite(col = "cell", dplyr::all_of(vars), sep = ", ") |>
      dplyr::mutate(
        variables = paste(vars, collapse = ", "),
        abs_diff = abs(.data$prop_synth - .data$prop_conf)
      ) |>
      dplyr::select(
        "variables", "cell", "prop_synth", "prop_conf", "abs_diff"
      )
    # variables disambiguates cells across combinations and drives the
    # per-combination summary; the prop columns show the direction of the
    # discrepancy, not just its size
    return(cells)
  }

  # all per-cell differences for one (synth, conf) pair of datasets
  compute_cells <- function(synth_d, conf_d, allow_empty_synth) {
    cells <- purrr::map(
      .x = seq_len(nrow(kmarginals_vars)),
      .f = \(i) marginal_cells(
        vars = kmarginals_vars[i, ],
        synth_d = synth_d,
        conf_d = conf_d,
        allow_empty_synth = allow_empty_synth
      )
    ) |>
      purrr::list_rbind()

    return(cells)
  }

  if (is.null(group_by)) {
    cells <- compute_cells(
      synth_d = synth_data, conf_d = conf_data, allow_empty_synth = FALSE
    ) |>
      dplyr::arrange(dplyr::desc(.data$abs_diff))

    # MabsDD per combination, worst marginals first
    marginals <- cells |>
      dplyr::summarize(madd = mean(.data$abs_diff), .by = "variables") |>
      dplyr::arrange(dplyr::desc(.data$madd))

    # mean of the MabsDDs, rescaled to an ascending measure on [0, 1000];
    # computed from all marginals before any truncation
    score <- (1 - mean(marginals$madd)) * 1000

    result <- structure(
      list(
        score = score,
        marginals = utils::head(marginals, n = keep_marginals),
        cells = utils::head(cells, n = keep_cells)
      ),
      class = "k_marginals"
    )

    return(result)
  }

  # strata are defined by the confidential data; a stratum with no synthetic
  # rows scores against all-zero synthetic proportions. Shares are the
  # confidential row (or weight) share of each stratum, computed once
  conf_totals <- if (is.null(weight_var)) {
    rep(1, nrow(conf_data))
  } else {
    conf_data[[weight_var]]
  }

  strata <- conf_data |>
    dplyr::mutate(.stratum_total = conf_totals) |>
    dplyr::summarize(
      .share = sum(.data$.stratum_total),
      .by = dplyr::all_of(group_by)
    ) |>
    dplyr::mutate(.share = .data$.share / sum(.data$.share))

  per_stratum <- purrr::map(
    .x = seq_len(nrow(strata)),
    .f = \(i) {
      stratum <- strata[i, group_by, drop = FALSE]

      synth_g <- dplyr::semi_join(synth_data, stratum, by = group_by)
      conf_g <- dplyr::semi_join(conf_data, stratum, by = group_by)

      cells_g <- compute_cells(
        synth_d = synth_g, conf_d = conf_g, allow_empty_synth = TRUE
      )

      marginals_g <- cells_g |>
        dplyr::summarize(madd = mean(.data$abs_diff), .by = "variables")

      list(
        cells = dplyr::bind_cols(stratum, cells_g),
        marginals = dplyr::bind_cols(stratum, marginals_g),
        group_scores = dplyr::bind_cols(
          stratum,
          tibble::tibble(
            share = strata$.share[i],
            score = (1 - mean(marginals_g$madd)) * 1000
          )
        )
      )
    }
  )

  cells <- purrr::list_rbind(purrr::map(per_stratum, "cells")) |>
    dplyr::arrange(dplyr::desc(.data$abs_diff))

  marginals <- purrr::list_rbind(purrr::map(per_stratum, "marginals")) |>
    dplyr::arrange(dplyr::desc(.data$madd))

  group_scores <- purrr::list_rbind(
    purrr::map(per_stratum, "group_scores")
  ) |>
    dplyr::arrange(.data$score)

  # per-stratum scores roll up weighted by confidential shares, so small
  # strata surface in group_scores without dominating the headline
  score <- sum(group_scores$share * group_scores$score)

  result <- structure(
    list(
      score = score,
      marginals = utils::head(marginals, n = keep_marginals),
      cells = utils::head(cells, n = keep_cells),
      group_scores = group_scores
    ),
    class = "k_marginals"
  )

  return(result)
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
#' MabsDDs are averaged across combinations and rescaled as (1 - mean) * 1000.
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
#' range [0, 1000] where a higher value denotes lower MabsDDs and consequently
#' greater similarity between confidential and synthetic data; `marginals`, a
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
  k = 3,
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

  # NULL for plain data frame eval_data, so the worker applies no restriction
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
