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
  # validate weight_var
  if (!is.null(weight_var)) {

    stopifnot(
      "`weight_var` must be a single character string" = {
        rlang::is_string(weight_var)
      },
      "`weight_var` must be a column in both datasets" = {
        weight_var %in% names(synth_data) && weight_var %in% names(conf_data)
      }
    )

    for (weights in list(synth_data[[weight_var]], conf_data[[weight_var]])) {

      stopifnot(
        "`weight_var` must be a numeric column in both datasets" = {
          is.numeric(weights)
        },
        "`weight_var` values must be finite in both datasets" = {
          all(is.finite(weights))
        },
        "`weight_var` values must be non-negative in both datasets" = {
          all(weights >= 0)
        },
        "`weight_var` values must have a positive total in both datasets" = {
          sum(weights) > 0
        }
      )

    }

  }

  # validate group_by
  if (!is.null(group_by)) {

    stopifnot(
      "`group_by` must be a character vector of variables present in both datasets" = {
        is.character(group_by) &&
          length(group_by) >= 1 &&
          !anyNA(group_by) &&
          all(group_by %in% names(synth_data)) &&
          all(group_by %in% names(conf_data))
      },
      "`group_by` must not contain duplicate variable names" = {
        !anyDuplicated(group_by)
      },
      "`group_by` cannot include `weight_var`" = {
        is.null(weight_var) || !(weight_var %in% group_by)
      }
    )

  }

  # variables available for marginals: shared by both datasets, excluding
  # weights and grouping variables
  shared_vars <- setdiff(
    intersect(names(synth_data), names(conf_data)),
    c(weight_var, group_by)
  )

  # optionally restrict to synthesized variables
  if (!is.null(synth_varnames)) {

    stopifnot(
      "`synth_varnames` must be a non-empty character vector without missing values, or NULL" = {
        is.character(synth_varnames) &&
          length(synth_varnames) >= 1 &&
          !anyNA(synth_varnames)
      }
    )

    shared_vars <- intersect(shared_vars, synth_varnames)

    if (length(shared_vars) == 0) {

      stop(
        "`synth_varnames` matches no variables available for marginals ",
        "after shared-variable, `group_by`, and `weight_var` filtering"
      )

    }

  }

  if (length(shared_vars) < k) {

    stop(
      "`k` cannot exceed the number of variables available for marginals ",
      "after shared-variable, `group_by`, `weight_var`, and ",
      "`synth_varnames` filtering"
    )

  }

  # optionally discretize numeric variables
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

  # handle missing values in marginal and grouping variables
  na_vars_scope <- c(shared_vars, group_by)

  if (!na.rm) {

    # NA becomes its own level; report which variables are affected
    has_na <- purrr::map_lgl(
      .x = na_vars_scope,
      .f = \(v) anyNA(synth_data[[v]]) || anyNA(conf_data[[v]])
    )

    if (any(has_na)) {

      message(
        "Some variables contain missing data: ",
        paste(na_vars_scope[has_na], collapse = ", ")
      )

    }

    synth_data[na_vars_scope] <- convert_na_to_level(synth_data[na_vars_scope])
    conf_data[na_vars_scope] <- convert_na_to_level(conf_data[na_vars_scope])

  } else if (!is.null(group_by)) {

    # marginal variables are handled per combination downstream; grouping
    # variables must be dropped here so strata are well defined
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
