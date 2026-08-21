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

  .validate_bins(bins = bins)

  numeric_vars <- vars[
    purrr::map_lgl(.x = vars, .f = \(v) is.numeric(conf_data[[v]]))
  ]

  for (var in numeric_vars) {
    # cut points derive from observed values; missing values follow na.rm
    # like every other variable, becoming an NA bin or dropped rows
    conf_values <- conf_data[[var]][!is.na(conf_data[[var]])]

    .validate_discretizable_values(values = conf_values, var = var, bins = bins)

    cut_points <- .compute_cut_points(values = conf_values,
                                      bins = bins,
                                      discretize_method = discretize_method)

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

  return(list(synth_data = synth_data, conf_data = conf_data))

}

#' @title Input validation for the number of bins in 
#' .discretize_k_marginal_vars()
#'
#' @description
#' The function returns an error when any of the conditions are met:
#' 1. bins is not a numeric data type
#' 2. bins is not a scalar
#' 3. bins is an NA value
#' 4. bins is lesser than 2
#' 5. bins is not an integer
#'
#' @param bins numeric scalar for number of bins for data
#'
#' @return `TRUE` if `bins` is a single integer >= 2; otherwise an error is
#' thrown.
#'
.validate_bins <- function(bins) {

  if (!(is.numeric(bins) &&
          length(bins) == 1 &&
          !is.na(bins) &&
          bins >= 2 &&
          bins == floor(bins))) {

    stop("`bins` must be a single integer >= 2")

  } else {

    return(TRUE)

  }

}

#' @title Validate values for discretization into bins
#'
#' @description Validates that a numeric variable has finite observed values
#' and enough distinct values to construct the requested number of bins.
#'
#' @param values Numeric vector of observed confidential values for one
#' variable.
#' @param var Character scalar naming the variable.
#' @param bins Single integer >= 2 giving the requested number of bins.
#'
#' @return `TRUE` if validation passes; otherwise an error is thrown.
#'
.validate_discretizable_values <- function(values, var, bins) {

  if (!all(is.finite(values)) || length(values) == 0) {

    stop(
      "observed numeric values must be finite to discretize; `", var,
      "` is not"
    )

  }

  if (dplyr::n_distinct(values) < bins) {

    stop(
      "`", var, "` has fewer distinct confidential values than `bins`"
    )

  }

  return(TRUE)

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
#' @return A numeric vector of `bins - 1` interior cut points. If
#' `discretize_method` is not one of `"width"`, `"ntile"`, or `"cluster"`,
#' an error is thrown.
#'
.compute_cut_points <- function(values, bins, discretize_method) {

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

  } else {

    stop(
      "`discretize_method` must be one of 'width', 'ntile', or 'cluster'"
    )

  }

  return(cut_points)

}