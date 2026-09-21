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

  var_order <- x |>
    dplyr::select(tidyselect::where(is.numeric)) |>
    names()

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
        tidyr::unnest("cor")

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
        tidyr::unnest("cov")

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
  if (is.matrix(matrix)) {
    matrix <- tibble::as_tibble(matrix, rownames = "var1")
  }

  id_cols <- intersect(c("var1", group_by_q), names(matrix))

  matrix <- matrix |>
    tidyr::pivot_longer(
      cols = -dplyr::all_of(id_cols),
      names_to = "var2",
      values_to = "statistic"
    ) |>
    dplyr::select(dplyr::any_of(c(group_by_q, "var1", "var2", "statistic")))

  # for correlation matrices only, delete diagonal
  # for covariance matrix, keep diagonal
  if (statistic == "correlation") {
    matrix <- matrix |>
      dplyr::filter(match(.data$var1, var_order) > match(.data$var2, var_order)
    )
  } else if (statistic == "covariance") {
    matrix <- matrix |>
      dplyr::filter(var1 >= var2)
  }

  return(matrix)

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

#' Calculate relative mutual information
#'
#' @param df a dataframe containing factor variables
#' @param group_by_q optional quoted character string of a variable name to
#' group the data by. If provided, the correlation matrix will be calculated
#' for each group separately.
#' @return A long tibble with relative mutual information
#
.calc_rmi_tibble <- function(df, group_by_q) {

  calc_one_rmi <- function(df) {
    p <- ncol(df)

    rmi_matrix <- matrix(0, nrow = p, ncol = p)
    for (col in seq_len(p)) {
      for (row in seq_len(p)) {
        rmi_matrix[row, col] <- .relative_mutual_information(
          x = df[, row, drop = TRUE],
          y = df[, col, drop = TRUE]
        )
      }
    }

    rownames(rmi_matrix) <- names(df)
    colnames(rmi_matrix) <- names(df)

    out <- rmi_matrix |>
      tibble::as_tibble(rownames = "var1") |>
      tidyr::pivot_longer(
        cols = -dplyr::all_of("var1"),
        names_to = "var2",
        values_to = "statistic"
      ) |>
      dplyr::filter(.data$var1 != .data$var2)

    return(out)
  }

  if (is.null(group_by_q)) {

    return(calc_one_rmi(df))

  } else {

    grouped_df <- df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_by_q)))

    keys <- dplyr::group_keys(grouped_df)
    list_of_df <- dplyr::group_split(grouped_df, .keep = FALSE)

    list_of_tibbles <- purrr::map2_dfr(
      .x = list_of_df,
      .y = seq_along(list_of_df),
      .f = \(dat, i) {
        dplyr::bind_cols(keys[i, , drop = FALSE], calc_one_rmi(dat))
      }
    )

    return(list_of_tibbles)

  }

}


#' Calculate bivariate statistics  of a confidential data set.
#'
#' @param synth_data A data.frame with synthetic data
#' @param conf_data A data.frame with the confidential data
#' @param statistic a character string specifying which bivariate statistic
#' to be returned by the function. One of "correlation", "covariance", or "RMI"
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

  # parameter validation
  valid_statistics <- c("correlation", "covariance", "rmi")
  na.statistic <- pmatch(x = tolower(statistic), table = valid_statistics)
  if (is.na(na.statistic)) stop("invalid 'statistic' argument")
  statistic <- match.arg(arg = tolower(statistic), choice = valid_statistics)

  if (statistic == "rmi" && (!is.null(method) || !is.null(use))) message("NOTE: RMI ignores the 'method' and 'use' arguments")

  # Create list of variables to subset synth_data and conf_data
  # First, get numeric variables present in both data sets

  if (statistic %in% c("correlation", "covariance")) {
    intersect_vars <- intersect(
      synth_data |>
        dplyr::select(tidyselect::where(is.numeric)) |>
        names(),
      conf_data |>
        dplyr::select(tidyselect::where(is.numeric)) |>
        names()
    )
  } else {
    intersect_vars <- intersect(
      synth_data |>
        dplyr::select(tidyselect::where(is.factor)) |>
        names(),
      conf_data |>
        dplyr::select(tidyselect::where(is.factor)) |>
        names()
    )
  }

  # build names for returned list
  name_original <- paste0(statistic, "_original")
  name_synthetic <- paste0(statistic, "_synthetic")
  name_difference <- paste0(statistic, "_difference")
  name_fit <- paste0(statistic, "_fit")
  name_mae <- paste0(statistic, "_difference_mae")
  name_rmse <- paste0(statistic, "_difference_rmse")

  # Edge case: correlation and covariance need at least 2 numeric variables
  if (length(intersect_vars) < 2) {
    empty_tibble <- tibble::tibble(var1 = character(), var2 = character())
    empty_tibble[[statistic]] <- numeric()

    # add empty group_by_q column with correct type when provided
    if (!is.null(group_by_q)) {
      group_cols_empty <- conf_data |>
        dplyr::select(dplyr::all_of(group_by_q)) |>
        dplyr::slice(0)
      empty_tibble <- dplyr::bind_cols(group_cols_empty, empty_tibble)
    }

    empty_tibble_diff <- empty_tibble |>
      dplyr::rename(difference = dplyr::all_of(statistic))

    out <- list(
      empty_tibble,
      empty_tibble,
      empty_tibble_diff,
      NA_real_,
      NA_real_,
      NA_real_
    )
    names(out) <- c(name_original, name_synthetic, name_difference, name_fit, name_mae, name_rmse)
    return(out)
  }


  # Second, add group_by variables to the list if supplied
  if (!is.null(group_by_q)) {
    vars_select <- c(intersect_vars, group_by_q)
  } else {
    vars_select <- intersect_vars
  }

  # reorder data names
  synth_data <- dplyr::select(synth_data, dplyr::all_of(vars_select))
  conf_data <- dplyr::select(conf_data, dplyr::all_of(vars_select))

  if (statistic %in% c("correlation", "covariance")) {
    # find the lower triangle of the original data linear correlation matrix
    original_lt <- .lower_triangle(conf_data, use = use, group_by_q = group_by_q, method = method, statistic = statistic)

    # find the lower triangle of the synthetic data linear correlation matrix
    synthetic_lt <- .lower_triangle(synth_data, use = use, group_by_q = group_by_q, method = method, statistic = statistic)
  } else if (statistic == "rmi") {
    original_lt <- .calc_rmi_tibble(conf_data, group_by_q = group_by_q)
    synthetic_lt <- .calc_rmi_tibble(synth_data, group_by_q = group_by_q)
  }

  
  # find the difference between the matrices
  difference_lt <-
    dplyr::full_join(
      original_lt,
      synthetic_lt,
      by = c("var1", "var2", group_by_q),
      suffix = c("_original", "_synthetic")
    ) |>
    dplyr::mutate(difference = .data$statistic_synthetic - .data$statistic_original) |>
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
        fit = dplyr::case_when(
          n == 0 ~ NA_real_,
          sse == 0 ~ 0,
          n_nonzero_cells == 0 ~ NA_real_,
          TRUE ~ sqrt(sse) / n_nonzero_cells
        ),
        difference_mae = if (n == 0) {
          NA_real_
        } else {
          mean(abs(.data$difference), na.rm = TRUE)
        },
        difference_rmse = if (n == 0) {
          NA_real_
        } else {
          sqrt(mean(.data$difference ^ 2, na.rm = TRUE))
        },
        .groups = "drop"
      )

    fit <- metrics |>
      dplyr::select(dplyr::any_of(group_by_q), fit)

    difference_mae <- metrics |>
      dplyr::select(dplyr::any_of(group_by_q), difference_mae)

    difference_rmse <- metrics |>
      dplyr::select(dplyr::any_of(group_by_q), difference_rmse)

  } else {

    n <- sum(!is.na(difference_lt$difference))
    sse <- sum(difference_lt$difference ^ 2, na.rm = TRUE)
    fit <- dplyr::case_when(
      n == 0 ~ NA_real_,
      sse == 0 ~ 0,
      n_nonzero_cells == 0 ~ NA_real_,
      TRUE ~ sqrt(sse) / n_nonzero_cells
    )
    difference_vec <- difference_lt$difference[!is.na(difference_lt$difference)]
    difference_mae <- if (length(difference_vec) == 0) {
      NA_real_
    } else {
      mean(abs(difference_lt$difference), na.rm = TRUE)
    }
    difference_rmse <- if (length(difference_vec) == 0) {
      NA_real_
    } else {
      sqrt(mean(difference_lt$difference ^ 2, na.rm = TRUE))
    }
  }

  # now that we're done with operations, convert all the data frames to tibbles for consistency
  original_lt <- tibble::as_tibble(original_lt)
  synthetic_lt <- tibble::as_tibble(synthetic_lt)
  difference_lt <- tibble::as_tibble(difference_lt)

  names(original_lt)[names(original_lt) == "statistic"] <- statistic
  names(synthetic_lt)[names(synthetic_lt) == "statistic"] <- statistic

  out <- list(
    original = original_lt,
    synthetic = synthetic_lt,
    difference = difference_lt,
    fit = fit,
    difference_mae = difference_mae,
    difference_rmse = difference_rmse
  )
  names(out) <- c(name_original, name_synthetic, name_difference, name_fit, name_mae, name_rmse)
  return(out)

}

#' Calculate the covariance matrix of a confidential data set.
#'
#' @param eval_data An `eval_data` object
#' @param statistic a character string specifying which bivariate statistic
#' to be returned by the function. One of "correlation", "covariance", or "RMI"
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
util_bivariate <- function(eval_data, statistic, use = "everything", group_by_q = NULL, method = "pearson") {

  stopifnot(is_eval_data(eval_data))

  if (eval_data$n_rep == 1) {

    return(
      .util_bivariate(
        conf_data = eval_data$conf_data,
        synth_data = eval_data$synth_data,
        statistic = statistic,
        use = use,
        group_by_q = group_by_q,
        method = method
      )
    )

  } else {

    result <- purrr::map(
      .x = eval_data$synth_data,
      .f = \(sd) {

        .util_bivariate(
          conf_data = eval_data$conf_data,
          synth_data = sd,
          statistic = statistic,
          use = use,
          group_by_q = group_by_q,
          method = method
        )

      }
    )

    return(result)

  }

}
