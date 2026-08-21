#' @title Prepare inputs for the k-marginals helper
#'
#' @description Validates the data-dependent helper arguments, resolves the
#' shared marginal variables, optionally discretizes numeric variables, and
#' applies the requested missing-data handling before marginal computation.
#'
#' @param synth_data A tibble with synthetic data.
#' @param conf_data A tibble with confidential data.
#' @param k Scalar order of the k-marginal.
#' @param weight_var Optional character name of a numeric sample-weight
#' column present in both datasets.
#' @param group_by Optional character vector of grouping variable names
#' present in both datasets.
#' @param synth_varnames Optional character vector of synthesized variable
#' names.
#' @param na.rm A logical for ignoring `NA` values in proportion
#' calculations.
#' @param bins Optional single integer >= 2 for discretizing numeric shared
#' variables.
#' @param discretize_method Method used to place bin breaks when `bins` is
#' set.
#'
#' @return A list with `synth_data`, `conf_data`, and `shared_vars` ready for
#' downstream k-marginals computation.
#'
.prepare_k_marginals_inputs <- function(
  synth_data,
  conf_data,
  k,
  weight_var = NULL,
  group_by = NULL,
  synth_varnames = NULL,
  na.rm = FALSE,
  bins = NULL,
  discretize_method = c("width", "ntile", "cluster")
) {
  if (!is.null(weight_var)) {

    is_character_weight_var <- is.character(weight_var)
    has_single_weight_var <- length(weight_var) == 1

    if (!(
      is_character_weight_var &&
      has_single_weight_var
    )) {

      stop("`weight_var` must be a single character string")

    }

    synth_has_weight_var <- weight_var %in% names(synth_data)
    conf_has_weight_var <- weight_var %in% names(conf_data)

    if (!(
      synth_has_weight_var &&
      conf_has_weight_var
    )) {

      stop("`weight_var` must be a column in both datasets")

    }

    synth_weight_is_numeric <- is.numeric(synth_data[[weight_var]])
    conf_weight_is_numeric <- is.numeric(conf_data[[weight_var]])

    if (!(
      synth_weight_is_numeric &&
      conf_weight_is_numeric
    )) {

      stop("`weight_var` must be a numeric column in both datasets")

    }

    for (weights in list(synth_data[[weight_var]], conf_data[[weight_var]])) {

      has_finite_weights <- all(is.finite(weights))
      has_nonnegative_weights <- all(weights >= 0)
      has_positive_total_weight <- sum(weights) > 0

      if (!(
        has_finite_weights &&
        has_nonnegative_weights &&
        has_positive_total_weight
      )) {

        stop(
          "`weight_var` values must be finite and non-negative with a ",
          "positive total in both datasets"
        )

      }
    }
  }

  if (!is.null(group_by)) {
    is_character_group_by <- is.character(group_by)
    has_group_by_entries <- length(group_by) >= 1
    has_observed_group_by <- !anyNA(group_by)
    synth_has_group_by <- all(group_by %in% names(synth_data))
    conf_has_group_by <- all(group_by %in% names(conf_data))

    if (!(
      is_character_group_by &&
      has_group_by_entries &&
      has_observed_group_by &&
      synth_has_group_by &&
      conf_has_group_by
    )) {

      stop(
        "`group_by` must be a character vector of variables present in ",
        "both datasets"
      )

    }

    if (anyDuplicated(group_by) > 0) {

      stop("`group_by` must not contain duplicate variable names")

    }

    if (!(
      is.null(weight_var) ||
      !(weight_var %in% group_by)
    )) {

      stop("`group_by` cannot include `weight_var`")

    }
  }

  shared_vars <- setdiff(
    intersect(names(synth_data), names(conf_data)),
    c(weight_var, group_by)
  )

  if (!is.null(synth_varnames)) {
    is_character_synth_varnames <- is.character(synth_varnames)
    has_synth_varnames_entries <- length(synth_varnames) >= 1
    has_observed_synth_varnames <- !anyNA(synth_varnames)

    if (!(
      is_character_synth_varnames &&
      has_synth_varnames_entries &&
      has_observed_synth_varnames
    )) {

      stop(
        "`synth_varnames` must be a non-empty character vector without ",
        "missing values, or NULL"
      )

    }

    shared_vars <- intersect(shared_vars, synth_varnames)

    if (length(shared_vars) == 0) {

      stop(
        "`synth_varnames` matches no variables available for marginals ",
        "after shared-variable, `group_by`, and `weight_var` filtering"
      )

    }
  }

  has_enough_shared_vars <- length(shared_vars) >= k

  if (!has_enough_shared_vars) {

    stop(
      "`k` cannot exceed the number of variables available for marginals ",
      "after shared-variable, `group_by`, `weight_var`, and ",
      "`synth_varnames` filtering"
    )

  }

  if (!is.null(bins)) {
    discretized <- .discretize_k_marginal_vars(
      synth_data = synth_data,
      conf_data = conf_data,
      vars = shared_vars,
      bins = bins,
      discretize_method = discretize_method
    )

    synth_data <- discretized$synth_data
    conf_data <- discretized$conf_data
  }

  na_vars_scope <- c(shared_vars, group_by)

  if (!na.rm) {

    na_vars <- na_vars_scope[
      purrr::map_lgl(
        .x = na_vars_scope,
        .f = \(v) {
          anyNA(synth_data[[v]]) || anyNA(conf_data[[v]])
        }
      )
    ]

    if (length(na_vars) > 0) {

      message(
        "Some variables contain missing data: ",
        paste(na_vars, collapse = ", ")
      )

    }

    synth_data[na_vars_scope] <- convert_na_to_level(synth_data[na_vars_scope])
    conf_data[na_vars_scope] <- convert_na_to_level(conf_data[na_vars_scope])
  } else if (!is.null(group_by)) {

    synth_data <- dplyr::filter(
      synth_data,
      !dplyr::if_any(.cols = dplyr::all_of(group_by), .fns = is.na)
    )

    conf_data <- dplyr::filter(
      conf_data,
      !dplyr::if_any(.cols = dplyr::all_of(group_by), .fns = is.na)
    )

    if (nrow(conf_data) == 0) {

      stop(
        "no confidential rows remain after removing missing `group_by` values"
      )

    }
  }

  return(
    list(
      synth_data = synth_data,
      conf_data = conf_data,
      shared_vars = shared_vars
    )
  )
}