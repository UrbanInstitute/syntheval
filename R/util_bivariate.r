#' Calculate the lower triangle of a correlation or covariance matrix for
#' numeric random variables.
#'
#' @param x A data.frame
#' @param statistic a character string specifying which bivariate statistic
#' to be returned by the function. One of "correlation" or "covariance"
#' @param use optional character string giving a method for computing
#' covariances in the presence of missing values. This must be (an abbreviation
#' of) one of the strings "everything", "all.obs", "complete.obs",
#' "na.or.complete", or "pairwise.complete.obs".
#' @param method optional character string indicating which correlation
#' coefficient is to be computed. One of "pearson" (default),
#' "kendall", or "spearman".
#' @param group_by_q optional quoted character string of a variable name to
#' group the data by. If provided, the correlation matrix will be calculated
#' for each group separately.
#'
#' @return A data.frame with columns for deduplicated variable pairs and their
#' correlation or covariance values. Diagonal variable pairs (e.g. var1 = a
#' & var2 = a) are dropped from correlation matrices and preserved in
#' covariance matrices

.lower_triangle <- function(x, statistic, use, group_by_q = NULL, method = "pearson") {

  # parameter validation
  valid_statistics <- c("correlation", "covariance")
  na.statistic <- pmatch(x = statistic, table = valid_statistics)
  if (is.na(na.statistic)) stop("invalid 'statistic' argument")
  statistic <- match.arg(arg = statistic, choice = valid_statistics)

  # find the linear correlation or covariance matrix of numeric variables from a data set
  if (!is.null(group_by_q)) {

    matrix <- x |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_by_q)))

    if (statistic == "correlation") {

      matrix <- matrix |>
        dplyr::summarise(
          cor = list({
            m <-
              stats::cor(
                dplyr::pick(tidyselect::where(is.numeric)),
                use = use,
                method = method
              )
            tibble::as_tibble(m, rownames = "var1")
          }),
          .groups = "drop"
        ) |>
        tidyr::unnest(cor)

    } else if (statistic == "covariance") {

      matrix <- matrix |>
        dplyr::summarise(
          cov = list({
            m <-
              stats::cov(
                dplyr::pick(tidyselect::where(is.numeric)),
                  use = use,
                  method = method
              )
            tibble::as_tibble(m, rownames = "var1")
          }),
          .groups = "drop"
        ) |>
        tidyr::unnest(cov)

    }

  } else {

    # ungrouped version
    matrix <- x |> dplyr::select(tidyselect::where(is.numeric))
    if (statistic == "correlation") {
      matrix <- matrix |>
        stats::cor(use = use, method = method)
    } else if (statistic == "covariance") {
      matrix <- matrix |>
        stats::cov(use = use, method = method)
    }
  }

  # convert correlation matrix to long format to facilitate
  # comparisons between original and synthetic data correlation matrices
  # that are robust to the presence of grouping variables

  # handle matrix (no groups) vs tibble (grouped) uniformly
  if (is.matrix(cmatrix)) {
    matrix <- tibble::as_tibble(matrix, rownames = "var1")
  }

  id_cols <- intersect(c("var1", group_by_q), names(matrix))

matrix <- matrix |>
    tidyr::pivot_longer(
      cols = -dplyr::all_of(id_cols),
      names_to = "var2",
      values_to = statistic
    ) |>
    dplyr::select(dplyr::any_of(c(group_by_q, "var1", "var2", statistic))) |>
    dplyr::filter(var1 != var2)

  # for correlation matrices only, delete diagonal

  if (statistic == "correlation") {
    matrix <- matrix |>
      dplyr::filter(var1 == var2)
  }

  # both correlation and covariance, delete duplicate pairings
  matrix <- matrix |>
    dplyr::filter(var1 > var2)

  return(correlation_matrix)

}

#' Calculate entropy for RMI matrix
#'
#' @param p A vector of numeric probabilities
#' 
#' @return a numeric value for entropy of probabilities

.rmi_entropy <- function(p) {

  # drop zero probabilities
  p_positive <- p[p > 0]

  # calculate entropy
  entropy <- -sum(p_positive * log2(p_positive))

  return(entropy)

}

#' Calculate relative mutual information
#'
#' @param x A numeric or factor vector
#' @param y A numeric or factor vector
#'
#' @return A numeric relative mutual information between 0 and 1

.relative_mutual_information <- function(x, y) {

  Px <- unname(prop.table(table(x, useNA = "ifany")))
  Py <- unname(prop.table(table(y, useNA = "ifany")))
  Pxy <- unname(prop.table(table(x, y, useNA = "ifany")))

  mutual_info <- .rmi_entropy(Px) + .rmi_entropy(Py) - .rmi_entropy(as.vector(Pxy))

  relative_mutual_info <- mutual_info / .rmi_entropy(Px)

  return(relative_mutual_info)

}


#' Calculate bivariate statistics  of a confidential data set.
#'
#' @param synth_data A data.frame with synthetic data
#' @param conf_data A data.frame with the confidential data
#' @param use optional character string giving a method for computing
#' covariances in the presence of missing values. This must be (an abbreviation
#' of) one of the strings "everything", "all.obs", "complete.obs",
#' "na.or.complete", or "pairwise.complete.obs".
#' @param method optional character string indicating which covariance
#' is to be computed. One of "pearson" (default), "kendall", or "spearman".
#' @param group_by_q optional quoted character string of a variable name to
#' group the data by. If provided, the covariance matrix will be calculated
#' for each group separately.
#'
#' @return A long dataset with var1, var2, group_by_q if provided, and covariance

.util_bivariate <- function(synth_data = synth_data, conf_data,
                            statistic, use = "everything",
                            group_by_q = NULL, method = "pearson") {

  # Create list of variables to subset synth_data and conf_data
  # First, get numeric variables present in both data sets
  intersect_numeric <- intersect(
    synth_data |>
      dplyr::select(tidyselect::where(is.numeric)) |>
      names(),
    conf_data |>
      dplyr::select(tidyselect::where(is.numeric)) |>
      names()
  )
  # Second, add group_by variables to the list if supplied
  if (!is.null(group_by_q)) {
    vars_select <- c(intersect_numeric, group_by_q)
  } else {
    vars_select <- intersect_numeric
  }

  # reorder data names
  synth_data <- dplyr::select(synth_data, dplyr::all_of(vars_select))
  conf_data <- dplyr::select(conf_data, dplyr::all_of(vars_select))

  # find the lower triangle of the original data linear correlation matrix
  original_lt <- .lower_triangle(conf_data, use = use, group_by_q = group_by_q, method = method, statistic = statistic)

  # find the lower triangle of the synthetic data linear correlation matrix
  synthetic_lt <- .lower_triangle(synth_data, use = use, group_by_q = group_by_q, method = method, statistic = statistic)

  # check that the variable pairs in the original and synthetic data correlation matrices are the same
  # replaces previous check on rownames and colnames of correlation matrices
  original_pairs <- original_lt |>
    dplyr::distinct(var1, var2)

  synthetic_pairs <- synthetic_lt |>
    dplyr::distinct(var1, var2)

  if (!dplyr::setequal(original_pairs, synthetic_pairs)) {
    stop("The variable pairs in the original and synthetic data correlation matrices do not match.")
  }

  # find the difference between the matrices
  difference_lt <-
    dplyr::left_join(
      original_lt,
      synthetic_lt,
      by = c("var1", "var2", group_by_q),
      suffix = c("_original", "_synthetic")
    ) |>
    dplyr::mutate(difference = correlation_synthetic - correlation_original) |>
    dplyr::select(dplyr::any_of(c(group_by_q, "var1", "var2", "difference")))

  if (!is.null(group_by_q)) {

    metrics <- difference_lt |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_by_q))) |>
      dplyr::summarise(
        n = sum(!is.na(difference)),
        correlation_fit = dplyr::if_else(n == 0, NA_real_, sqrt(sum(difference ^ 2, na.rm = TRUE)) / n),
        correlation_difference_mae = mean(abs(difference), na.rm = TRUE),
        correlation_difference_rmse = sqrt(mean(difference ^ 2, na.rm = TRUE)),
        .groups = "drop"
      )

    correlation_fit <- metrics |>
      dplyr::select(dplyr::any_of(group_by_q), correlation_fit)

    correlation_difference_mae <- metrics |>
      dplyr::select(dplyr::any_of(group_by_q), correlation_difference_mae)

    correlation_difference_rmse <- metrics |>
      dplyr::select(dplyr::any_of(group_by_q), correlation_difference_rmse)

  } else {

    n <- sum(!is.na(difference_lt$difference))
    if (n == 0) {
      correlation_fit <- NA_real_
    } else {
      correlation_fit <- sqrt(sum(difference_lt$difference ^ 2, na.rm = TRUE)) / n
    }
    difference_vec <- difference_lt$difference[!is.na(difference_lt$difference)]
    correlation_difference_mae <- mean(abs(difference_vec))
    correlation_difference_rmse <- sqrt(mean(difference_vec ^ 2))

  }

  # now that we're done with operations, convert all the data frames to tibbles for consistency
  original_lt <- tibble::as_tibble(original_lt)
  synthetic_lt <- tibble::as_tibble(synthetic_lt)
  difference_lt <- tibble::as_tibble(difference_lt)

  return(
    list(
      correlation_original = original_lt,
      correlation_synthetic = synthetic_lt,
      correlation_difference = difference_lt,
      correlation_fit = correlation_fit,
      correlation_difference_mae = correlation_difference_mae,
      correlation_difference_rmse = correlation_difference_rmse
    )
  )

}


}

#' Calculate the covariance matrix of a confidential data set.
#'
#' @param eval_data An `eval_data` object
#' @param use Optional character string giving a method for computing
#' covariances in the presence of missing values. This must be (an abbreviation
#' of) one of the strings "everything", "all.obs", "complete.obs",
#' "na.or.complete", or "pairwise.complete.obs".
#' @param group_by_q Optional quoted character string of a variable name to
#' group the data by. If provided, the covariance matrix will be calculated
#' for each group separately.
#' @param method optional character string indicating which covariance is to
#' be computed. One of "pearson" (default), "kendall", or
#' "spearman".
#'
#' @return A long dataset with var1, var2, group_by_q if provided, and covariance
#' 
#' @family utility metrics
#'
#' @export
#'
util_cov <- function(eval_data, use = "everything", group_by_q = NULL, method = "pearson") {

  stopifnot(is_eval_data(eval_data))

  if (eval_data$n_rep == 1) {

    return(
      .util_cov(
        conf_data = eval_data$conf_data,
        synth_data = eval_data$synth_data,
        use = use,
        group_by_q = group_by_q,
        method = method
      )
    )

  } else {

    result <- purrr::map(
      .x = eval_data$synth_data,
      .f = \(sd) {

        .util_cov(
          conf_data = eval_data$conf_data,
          synth_data = sd,
          use = use,
          group_by_q = group_by_q,
          method = method
        )

      }
    )

    return(result)

  }
}
