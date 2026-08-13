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
#'
#' @return A `k_marginals` object with three elements: `score`, a value in
#' range [0, 1000] where a higher value denotes lower MabsDDs and consequently
#' greater similarity between confidential and synthetic data; `marginals`, a
#' tibble with the MabsDD for each combination of variables, worst first; and
#' `cells`, a tibble with the synthetic and confidential proportions and their
#' absolute difference for every cell, worst first. `score` is always computed
#' from all marginals, even when `keep_marginals` or `keep_cells` truncate the
#' detail tables.
#'
.util_k_marginals <- function(
    synth_data,
    conf_data,
    k,
    keep_marginals = Inf,
    keep_cells = Inf) {

  for (keep in list(keep_marginals, keep_cells)) {

    if (!(is.numeric(keep) && length(keep) == 1 && !is.na(keep) &&
          keep >= 1 && keep == floor(keep))) {

      stop(
        "`keep_marginals` and `keep_cells` must be single integers >= 1 or Inf"
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
#'
#' @return A `k_marginals` object with three elements: `score`, a value in
#' range [0, 1000] where a higher value denotes lower MabsDDs and consequently
#' greater similarity between confidential and synthetic data; `marginals`, a
#' tibble with the MabsDD for each combination of variables, worst first; and
#' `cells`, a tibble with the synthetic and confidential proportions and their
#' absolute difference for every cell, worst first. `score` is always computed
#' from all marginals, even when `keep_marginals` or `keep_cells` truncate the
#' detail tables. For multiple replicates, a list of such objects, one per
#' replicate.
#'
#' @export
#'
util_k_marginals <- function(
    eval_data,
    k = 3,
    keep_marginals = Inf,
    keep_cells = Inf) {

  stopifnot(is_eval_data(eval_data))

  if (eval_data$n_rep == 1) {

    return(
      .util_k_marginals(
        synth_data = eval_data$synth_data,
        conf_data = eval_data$conf_data,
        k = k,
        keep_marginals = keep_marginals,
        keep_cells = keep_cells
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
          keep_cells = keep_cells
        )

      }
    )

    return(result)

  }

}
