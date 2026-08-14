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
#' @param synth_varnames Optional character vector of synthesized variable
#' names. When set, only these variables (intersected with the variables
#' shared by both datasets) contribute marginals. Defaults to `NULL`, which
#' places no restriction on the shared variables.
#' @param bins Optional single integer >= 2. When set, every numeric shared
#' variable is discretized into this many bins (fewer, with a warning, if
#' tied quantile cut points collapse) with breaks derived from the
#' confidential data and applied to both datasets; the outer bins extend to
#' +/-Inf so synthetic values outside the confidential range land in edge
#' bins. Defaults to `NULL` (no discretization).
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
#' truncate the detail tables.
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
    synth_varnames = NULL,
    bins = NULL,
    discretize_method = c("width", "ntile", "cluster")) {

  discretize_method <- match.arg(discretize_method)

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

  # only variables present in both datasets contribute marginals; the weight
  # column is never itself a marginal
  shared_vars <- setdiff(
    intersect(names(synth_data), names(conf_data)),
    weight_var
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
        "`synth_varnames` shares no variables with both datasets; no ",
        "marginals can be computed"
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

      conf_values <- conf_data[[var]]

      if (!all(is.finite(conf_values))) {

        stop(
          "numeric variables must be finite to discretize; `", var,
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
      conf_data[[var]] <- cut(x = conf_values, breaks = breaks)

    }

  }

  if (length(shared_vars) < k) {

    stop("`k` cannot exceed the number of variables shared by both datasets")

  }

  if (!is.null(priority_vars)) {

    if (!(is.character(priority_vars) &&
          all(priority_vars %in% shared_vars))) {

      stop(
        "`priority_vars` must be a character vector of variables available ",
        "for marginals after applying shared-variable and `synth_varnames` ",
        "filtering"
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
      sort(c(which(is_priority), sampled_rows)), , drop = FALSE
    ]

  }

  # cell proportions for one dataset over one set of variables; weighted
  # proportions are weight shares instead of row shares
  process_data <- function(data, vars, prop_name) {

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
  marginal_cells <- function(vars) {

    cells <- dplyr::full_join(
      process_data(data = synth_data, vars = vars, prop_name = "prop_synth"),
      process_data(data = conf_data, vars = vars, prop_name = "prop_conf"),
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

  # per-cell differences across all k-way marginals, worst cells first
  cells <- purrr::map(
    .x = seq_len(nrow(kmarginals_vars)),
    .f = \(i) marginal_cells(vars = kmarginals_vars[i, ])
  ) |>
    purrr::list_rbind() |>
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
#' @param synth_vars A logical for if only synthesized variables should
#' contribute marginals. Only meaningful when the `eval_data` records which
#' variables were synthesized (i.e., was built from a `postsynth`); for plain
#' data frames all shared variables are used regardless. Defaults to `TRUE`.
#' @param bins Optional single integer >= 2. When set, every numeric shared
#' variable is discretized into this many bins (fewer, with a warning, if
#' tied quantile cut points collapse) with breaks derived from the
#' confidential data and applied to both datasets; the outer bins extend to
#' +/-Inf so synthetic values outside the confidential range land in edge
#' bins. Defaults to `NULL` (no discretization).
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
#' truncate the detail tables. For multiple replicates, a list of such
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
    synth_vars = TRUE,
    bins = NULL,
    discretize_method = c("width", "ntile", "cluster")) {

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
        synth_varnames = synth_varnames,
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
          synth_varnames = synth_varnames,
          bins = bins,
          discretize_method = discretize_method
        )

      }
    )

    return(result)

  }

}
