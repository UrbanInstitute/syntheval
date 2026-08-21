#' @title Discretize numeric variables for the k-marginals metric
#'
#' @description Discretizes every numeric variable among `vars` into `bins`
#' bins with interior cut points derived from the observed (non-missing)
#' confidential values and applied to both datasets. The outer bins extend to
#' +/-Inf so synthetic values outside the confidential range land in edge
#' bins.
#'
#' @param synth_data A tibble with synthetic data.
#' @param conf_data A tibble with confidential data.
#' @param vars Character vector of candidate variables; only numeric ones are
#' discretized.
#' @param bins Single integer >= 2 giving the number of bins. Fewer bins are
#' produced, with a warning, if tied cut points collapse.
#' @param discretize_method Method used to place bin breaks: "width" for
#' fixed binwidths, "ntile" for quantile bins, or "cluster" for univariate
#' k-means clustering (set a seed before calling for reproducible clusters).
#'
#' @return A list with the discretized `synth_data` and `conf_data`.
#'
.discretize_k_marginal_vars <- function(
  synth_data,
  conf_data,
  vars,
  bins,
  discretize_method
) {

  stopifnot(
    "`bins` must be a single integer >= 2" = {
      rlang::is_scalar_integerish(bins) && !is.na(bins) && bins >= 2
    }
  )

  numeric_vars <- vars[
    purrr::map_lgl(.x = vars, .f = \(v) is.numeric(conf_data[[v]]))
  ]

  for (var in numeric_vars) {
    # cut points derive from observed values; missing values follow na.rm
    # like every other variable, becoming an NA bin or dropped rows
    conf_values <- conf_data[[var]][!is.na(conf_data[[var]])]

    stopifnot(
      "observed numeric values must be finite to discretize" = {
        length(conf_values) > 0 && all(is.finite(conf_values))
      }
    )

    bins_var <- .resolve_bins(values = conf_values, var = var, bins = bins)

    cut_points <- .compute_cut_points(values = conf_values,
                                      bins = bins_var,
                                      discretize_method = discretize_method)

    # outer bins extend to +/-Inf so out-of-range synthetic values land in
    # edge bins; unique() collapses ties from skewed quantiles
    breaks <- unique(c(-Inf, cut_points, Inf))

    if (length(breaks) - 1 < bins_var) {

      warning(
        "`", var, "` was discretized into ", length(breaks) - 1,
        " bins instead of ", bins_var, " because of tied cut points"
      )

    }

    synth_data[[var]] <- cut(x = synth_data[[var]], breaks = breaks)
    conf_data[[var]] <- cut(x = conf_data[[var]], breaks = breaks)

  }

  return(list(synth_data = synth_data, conf_data = conf_data))

}

#' @title Resolve the effective number of bins for one variable
#'
#' @description Resolves the number of bins to use for one numeric variable by
#' comparing the requested bin count to the number of distinct confidential
#' values. When a variable has fewer distinct confidential values than
#' requested bins, the distinct-value count is used instead and a warning is
#' issued.
#'
#' @param values Numeric vector of observed confidential values for one
#' variable, typically after removing missing values.
#' @param var Character scalar naming the variable.
#' @param bins Single integer >= 2 giving the requested number of bins.
#'
#' @return A single integer giving the effective number of bins for the
#' variable.
#'
.resolve_bins <- function(values, var, bins) {

  distinct_values <- dplyr::n_distinct(values)
  bins_var <- min(bins, distinct_values)

  if (bins_var < bins) {

    warning(
      "`", var, "` was discretized into ", bins_var,
      " bins instead of ", bins,
      " because it has only ", distinct_values,
      " distinct confidential values"
    )

  }

  return(bins_var)

}

#' @title Compute discretization cut points
#'
#' @description Computes the interior cut points used to discretize a numeric
#' variable into the requested number of bins. Cut points are derived from the
#' observed confidential values using one of three methods: equal-width bins,
#' quantile bins, or univariate k-means clustering.
#'
#' @param values Numeric vector of observed confidential values for one
#' variable, typically after removing missing values.
#' @param bins Single integer >= 2 giving the requested number of bins.
#' @param discretize_method Character scalar specifying how cut points are
#' placed: `"width"` for equal-width bins, `"ntile"` for quantile bins, or
#' `"cluster"` for univariate k-means clustering.
#'
#' @return A numeric vector of `bins - 1` interior cut points.
#'
.compute_cut_points <- function(values, bins, discretize_method) {

  if (bins == 1) {

    return(numeric(0))

  }

  # interior cut points always derive from the confidential data; each
  # method yields bins - 1 of them
  if (discretize_method == "width") {

    cut_points <- seq(
      from = min(values),
      to = max(values),
      length.out = bins + 1
    )[2:bins]

  } else if (discretize_method == "ntile") {

    cut_points <- stats::quantile(
      x = values,
      probs = seq(from = 0, to = 1, length.out = bins + 1),
      names = FALSE
    )[2:bins]

  } else if (discretize_method == "cluster") {

    centers <- sort(
      stats::kmeans(x = values, centers = bins)$centers[, 1]
    )

    cut_points <- (centers[-1] + centers[-length(centers)]) / 2

  }

  return(cut_points)

}