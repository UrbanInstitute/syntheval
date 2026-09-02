#' 
#' Calculate the correlation fit metric of a confidential data set.
#'
#' @param synth_data A data.frame with synthetic data
#' @param conf_data A data.frame with the confidential data
#' @param use optional character string giving a method for computing
#' covariances in the presence of missing values. This must be (an abbreviation
#' of) one of the strings "everything", "all.obs", "complete.obs",
#' "na.or.complete", or "pairwise.complete.obs".
#' @param group_by_q optional quoted character string of a variable name to
#' group the data by. If provided, the correlation fit metric will be calculated
#' for each group separately.
#'
#' @return A `list` of fit metrics:
#'  - `correlation_original`: correlation matrix of the original data.
#'  - `correlation_synthetic`: correlation matrix of the synthetic data.
#'  - `correlation_difference`: difference between `correlation_synthetic` and
#'  `correlation_original`.
#'  - `correlation_fit`: square root of the sum of squared differences between
#'  `correlation_synthetic` and `correlation_original`, divided by the number of
#'  cells in the correlation matrix.
#' 
.util_corr_fit <- function(synth_data, conf_data, use = "everything", group_by_q = NULL) {

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
  if(!is.null(group_by_q)) {
    vars_select <- c(intersect_numeric, group_by_q)
  } else {
    vars_select <- intersect_numeric
  }

  # reorder data names
  synth_data <- dplyr::select(synth_data, dplyr::all_of(vars_select))
  conf_data <- dplyr::select(conf_data, dplyr::all_of(vars_select))

  # helper function to find a correlation matrix with the upper tri set to zeros
  lower_triangle <- function(x, use) {

    # find the linear correlation matrix of numeric variables from a data set
    if (!is.null(group_by_q)) {

      correlation_matrix <-
        x |>
        dplyr::group_by(dplyr::across(dplyr::all_of(group_by_q))) |>
        dplyr::summarise(
          cor = list({
            m <-
              stats::cor(
                dplyr::pick(tidyselect::where(is.numeric)),
                use = use
              )
            tibble::as_tibble(m, rownames = "var1")
          }),
          .groups = "drop"
        ) |>
        tidyr::unnest(cor)

    } else {

      # ungrouped version
      correlation_matrix <-
        x |>
        dplyr::select(tidyselect::where(is.numeric)) |>
        stats::cor(use = use)

    }

    # convert correlation matrix to long format to facilitate
    # comparisons between original and synthetic data correlation matrices
    # that are robust to the presence of grouping variables

    # handle matrix (no groups) vs tibble (grouped) uniformly
    if (is.matrix(correlation_matrix)) {
      correlation_matrix <- tibble::as_tibble(correlation_matrix, rownames = "var1")
    }

    id_cols <- intersect(c("var1", group_by_q), names(correlation_matrix))

    correlation_matrix <- correlation_matrix |>
      tidyr::pivot_longer(
        cols = -dplyr::all_of(id_cols),
        names_to = "var2",
        values_to = "correlation"
      ) |>
      dplyr::select(dplyr::any_of(c(group_by_q, "var1", "var2", "correlation"))) |>
      dplyr::filter(var1 != var2)

    return(correlation_matrix)

  }

  # find the lower triangle of the original data linear correlation matrix
  original_lt <- lower_triangle(conf_data, use = use)

  # find the lower triangle of the synthetic data linear correlation matrix
  synthetic_lt <- lower_triangle(synth_data, use = use)

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

#' 
#' Calculate the correlation fit metric of a confidential data set.
#'
#' @param eval_data An `eval_data` object
#' @param use optional character string giving a method for computing
#' covariances in the presence of missing values. This must be (an abbreviation
#' of) one of the strings "everything", "all.obs", "complete.obs",
#' "na.or.complete", or "pairwise.complete.obs".
#'
#' @return A `list` of fit metrics (one per each synthetic data replicate):
#'  - `correlation_original`: correlation matrix of the original data.
#'  - `correlation_synthetic`: correlation matrix of the synthetic data.
#'  - `correlation_difference`: difference between `correlation_synthetic` and
#'  `correlation_original`.
#'  - `correlation_fit`: square root of the sum of squared differences between
#'  `correlation_synthetic` and `correlation_original`, divided by the number of
#'  cells in the correlation matrix.
#'
#' @family utility metrics
#'
#' @export
#'
util_corr_fit <- function(eval_data, use = "everything", group_by_q = NULL) {

  stopifnot(is_eval_data(eval_data))

  if (eval_data$n_rep == 1) {

    return(
      .util_corr_fit(
        conf_data = eval_data$conf_data,
        synth_data = eval_data$synth_data,
        use = use,
        group_by_q = group_by_q
      )
    )

  } else {

    result <- purrr::map(
      .x = eval_data$synth_data,
      .f = \(sd) {

        .util_corr_fit(
          conf_data = eval_data$conf_data,
          synth_data = sd,
          use = use,
          group_by_q = group_by_q
        )

      }
    )

    return(result)

  }
}
