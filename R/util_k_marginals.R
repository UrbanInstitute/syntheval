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
#'
#' @return A value in range [0, 1000] where a higher value denotes lower MabsDDs
#' and consequently greater similarity between confidential and synthetic data.
#'
.util_k_marginals <- function(synth_data, conf_data, k) {

  if (!(is.numeric(k) && length(k) == 1 && k %in% 1:3)) {

    stop("`k` must be a single integer between 1 and 3")

  }

  stopifnot(inherits(synth_data, "data.frame"))
  stopifnot(inherits(conf_data, "data.frame"))

  if (nrow(synth_data) == 0 || nrow(conf_data) == 0) {

    stop("`synth_data` and `conf_data` must each contain at least one row")

  }

  # only variables present in both datasets contribute marginals
  shared_vars <- intersect(names(synth_data), names(conf_data))

  if (length(shared_vars) < k) {

    stop("`k` cannot exceed the number of variables shared by both datasets")

  }

  kmarginals_vars <- t(utils::combn(x = shared_vars, m = k))

  # cell proportions for one dataset over one set of variables
  process_data <- function(data, vars, prop_name) {

    props <- data |>
      dplyr::select(dplyr::all_of(vars)) |>
      dplyr::group_by_all() |>
      dplyr::count() |>
      dplyr::ungroup() |>
      dplyr::mutate("{prop_name}" := .data$n / sum(.data$n)) |>
      dplyr::select(-"n")

    return(props)

  }
  
  # MabsDD for one set of variables; cells absent from one dataset count as 0
  madd <- function(vars) {

    combined_data <- dplyr::full_join(
      process_data(data = synth_data, vars = vars, prop_name = "prop_synth"),
      process_data(data = conf_data, vars = vars, prop_name = "prop_conf"),
      by = vars
    ) |>
      tidyr::replace_na(replace = list(prop_synth = 0, prop_conf = 0))

    madd <- combined_data |>
      dplyr::summarize(
        madd = mean(abs(.data$prop_synth - .data$prop_conf))
      ) |>
      dplyr::pull("madd")

    return(madd)

  }

  # iterate over all k-way marginals
  madds <- purrr::map_dbl(
    .x = seq_len(nrow(kmarginals_vars)),
    .f = \(i) madd(vars = kmarginals_vars[i, ])
  )

  # mean of the MabsDDs, rescaled to an ascending measure on [0, 1000]
  return((1 - mean(madds)) * 1000)

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
#'
#' @return A value in range [0, 1000] where a higher value denotes lower MabsDDs
#' and consequently greater similarity between confidential and synthetic data.
#' For multiple replicates, a list of such values, one per replicate.
#'
#' @export
#'
util_k_marginals <- function(eval_data, k = 3) {

  stopifnot(is_eval_data(eval_data))

  if (eval_data$n_rep == 1) {

    return(
      .util_k_marginals(
        synth_data = eval_data$synth_data,
        conf_data = eval_data$conf_data,
        k = k
      )
    )

  } else {

    result <- purrr::map(
      .x = eval_data$synth_data,
      .f = \(sd) {

        .util_k_marginals(
          synth_data = sd,
          conf_data = eval_data$conf_data,
          k = k
        )

      }
    )

    return(result)

  }

}
