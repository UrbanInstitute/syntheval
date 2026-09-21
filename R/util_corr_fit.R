#' Calculate the lower triangle of a correlation matrix for numeric random variables.
#' 
#' @param x A data.frame
#' @param use optional character string giving a method for computing
#' covariances in the presence of missing values. This must be (an abbreviation
#' of) one of the strings "everything", "all.obs", "complete.obs",
#' "na.or.complete", or "pairwise.complete.obs".
#' @param group_by_q optional quoted character string of a variable name to
#' group the data by. If provided, the correlations will be calculated
#' for each group separately
#' 
#' @return A data.frame with columns for the variable pairs and their correlation values.

.lower_triangle <- function(x, use, group_by_q = NULL) {

    var_order <- x |>
      dplyr::select(tidyselect::where(is.numeric)) |>
      names()

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
      tidyr::unnest("cor")

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

  # capture before pivot
  id_cols <- intersect(c("var1", group_by_q), names(correlation_matrix))

  correlation_matrix <- correlation_matrix |>
    tidyr::pivot_longer(
      cols = -dplyr::all_of(id_cols),
      names_to = "var2",
      values_to = "correlation"
    ) |>
    dplyr::select(dplyr::any_of(c(group_by_q, "var1", "var2", "correlation")))

  # delete duplicate correlations so it is a true lower triangle
  correlation_matrix <- correlation_matrix |>
    dplyr::filter(
      match(.data$var1, var_order) > match(.data$var2, var_order)
    )

  return(correlation_matrix)

}

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
#'  - `correlation_original`: correlation values from original data of pairs of
#'        numeric variables that appear in both original and synthetic datasets,
#'        formatted in a long tibble
#'  - `correlation_synthetic`: correlation values from synthetic data of pairs of
#'        numeric variables that appear in both original and synthetic datasets,
#'        formatted in a long tibble
#'  - `correlation_difference`: difference between `correlation_synthetic` and
#'  `correlation_original`, formatted in a long tibble.
#'  - `correlation_fit`: square root of the sum of squared differences between
#'  `correlation_synthetic` and `correlation_original`, divided by the number of
#'  cells in the complete correlation matrix prior to conversion to long tibbles

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

  # Edge case: correlation needs at least 2 numeric variables
  if (length(intersect_numeric) < 2) {
    empty_tibble <- tibble::tibble(var1 = character(), var2 = character(), correlation = numeric())

    # add empty group_by_q column with correct type when provided
    if (!is.null(group_by_q)) {
      group_cols_empty <- conf_data |>
        dplyr::select(dplyr::all_of(group_by_q)) |>
        dplyr::slice(0)
      empty_tibble <- dplyr::bind_cols(group_cols_empty, empty_tibble)
    }

    empty_tibble_diff <- empty_tibble |>
      dplyr::rename(difference = "correlation")

    return(list(
      correlation_original =  empty_tibble,
      correlation_synthetic =  empty_tibble,
      correlation_difference = empty_tibble_diff,
      correlation_fit = NA_real_,
      correlation_difference_mae = NA_real_,
      correlation_difference_rmse = NA_real_
    ))
  }

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
  original_lt <- .lower_triangle(conf_data, use = use, group_by_q = group_by_q)

  # find the lower triangle of the synthetic data linear correlation matrix
  synthetic_lt <- .lower_triangle(synth_data, use = use, group_by_q = group_by_q)

  # find the difference between the matrices
  difference_lt <-
    dplyr::full_join(
      original_lt,
      synthetic_lt,
      by = c("var1", "var2", group_by_q),
      suffix = c("_original", "_synthetic")
    ) |>
    dplyr::mutate(difference = .data$correlation_synthetic - .data$correlation_original) |>
    dplyr::select(dplyr::any_of(c(group_by_q, "var1", "var2", "difference")))

  # find the number of non-zero cells in the "lower triangle" for correlation_fit
  # (aka among unique non-diagonal pairs)
  # this matches the existing behavior of util_corr_fit
  n_nonzero_cells <- difference_lt |>
    dplyr::filter(.data$difference != 0) |>
    nrow()

  if (!is.null(group_by_q)) {

    metrics <- difference_lt |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_by_q))) |>
      dplyr::summarise(
        n = sum(!is.na(.data$difference)),
        # sum of squared errors
        sse = sum(.data$difference ^ 2, na.rm = TRUE),
        correlation_fit = dplyr::case_when(
          n == 0 ~ NA_real_,
          sse == 0 ~ 0,
          n_nonzero_cells == 0 ~ NA_real_,
          TRUE ~ sqrt(sse) / n_nonzero_cells
        ),
        correlation_difference_mae = if (n == 0) {
          NA_real_
        } else {
          mean(abs(.data$difference), na.rm = TRUE)
        },
        correlation_difference_rmse = if (n == 0) {
          NA_real_
        } else {
          sqrt(mean(.data$difference ^ 2, na.rm = TRUE))
        },
        .groups = "drop"
      ) |>
      dplyr::select(-dplyr::any_of("sse"))


    correlation_fit <- metrics |>
      dplyr::select(dplyr::any_of(c(group_by_q, "correlation_fit")))

    correlation_difference_mae <- metrics |>
      dplyr::select(dplyr::any_of(c(group_by_q, "correlation_difference_mae")))

    correlation_difference_rmse <- metrics |>
      dplyr::select(dplyr::any_of(c(group_by_q, "correlation_difference_rmse")))


  } else {

    n <- sum(!is.na(difference_lt$difference))
    sse <- sum(difference_lt$difference ^ 2, na.rm = TRUE)
    correlation_fit <- dplyr::case_when(
      n == 0 ~ NA_real_,
      sse == 0 ~ 0,
      n_nonzero_cells == 0 ~ NA_real_,
      TRUE ~ sqrt(sse) / n_nonzero_cells
    )
    difference_vec <- difference_lt$difference[!is.na(difference_lt$difference)]
    correlation_difference_mae <- if (length(difference_vec) == 0) {
      NA_real_
    } else {
      mean(abs(difference_lt$difference), na.rm = TRUE)
    }

    correlation_difference_rmse <- if (length(difference_vec) == 0) {
      NA_real_
    } else {
      sqrt(mean(difference_lt$difference ^ 2, na.rm = TRUE))
    }

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
#' @param group_by_q optional quoted character string of a variable name to
#' group the data by. If provided, the correlation fit metric will be calculated
#' for each group separately.
#'
#' @return A `list` of fit metrics (one per each synthetic data replicate):
#'  - `correlation_original`: correlation values from original data of pairs of
#'        numeric variables that appear in both original and synthetic datasets,
#'        formatted in a long tibble
#'  - `correlation_synthetic`: correlation values from synthetic data of pairs of
#'        numeric variables that appear in both original and synthetic datasets,
#'        formatted in a long tibble
#'  - `correlation_difference`: difference between `correlation_synthetic` and
#'  `correlation_original`.
#'  - `correlation_fit`: square root of the sum of squared differences between
#'  `correlation_synthetic` and `correlation_original`, divided by the number of
#'  cells in the complete correlation matrix prior to conversion to long tibbles
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