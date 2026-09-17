#' Calculate the covariance  of a confidential data set.
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

.util_cov <- function(synth_data = synth_data, conf_data, use = "everything", group_by_q = NULL, method = "pearson") {

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

  if (!is.null(group_by_q)) {

    conf_grouped <- conf_data |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_by_q)))

    synth_grouped <- synth_data |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_by_q)))

    conf_keys <- dplyr::group_keys(conf_grouped)
    synth_keys <- dplyr::group_keys(synth_grouped)

    if (!identical(conf_keys, synth_keys)) {
      stop("Grouping levels differ between conf_data and synth_data.")
    }

    list_of_conf <- dplyr::group_split(conf_grouped, .keep = FALSE)
    list_of_synth <- dplyr::group_split(synth_grouped, .keep = FALSE)

    group_labels <- conf_keys |>
      tidyr::unite(".label", dplyr::everything(), sep = " | ") |>
      dplyr::pull(".label")

    cov_mat <- purrr::map2(
      .x = list_of_conf,
      .y = list_of_synth,
      .f = \(x, y) stats::cov(x = x, y = y, use = use, method = method) |>
        tibble::as_tibble(rownames = "var1")
    )

    names(cov_mat) <- group_labels

  } else {

    cov_mat <- stats::cov(x = conf_data, y = synth_data, use = use, method = method) |>
      tibble::as_tibble(rownames = "var1")
  }

  if (!is.null(group_by_q)) {

    group_lookup <- conf_keys |>
      tidyr::unite(".group", dplyr::all_of(group_by_q), sep = " | ", remove = FALSE)

    cov_mat <- dplyr::bind_rows(cov_mat, .id = ".group") |>
      dplyr::left_join(group_lookup, by = ".group")

    id_cols <- c(".group", group_by_q, "var1")
  } else {
    id_cols <- "var1"
  }

  # create long dataset for plotting
  cov_long <- cov_mat |>
    tidyr::pivot_longer(
      cols = -dplyr::all_of(id_cols),
      names_to = "var2",
      values_to = "covariance"
    )

  # delete duplicate pairings
  cov_long <- cov_long[cov_long$var1 > cov_long$var2, , drop = FALSE]

  cov_long <- cov_long |>
    dplyr::select(dplyr::any_of(c(group_by_q, "var1", "var2", "covariance")))


  return(cov_long)

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
