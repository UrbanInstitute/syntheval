#' @title Compute per-cell proportion differences for the k-marginals metric
#'
#' @description For each supplied variable combination, computes the marginal
#' cell proportions in the synthetic and confidential data and their absolute
#' differences. Cells absent from one dataset count as proportion zero.
#'
#' @param synth_data A tibble with synthetic data.
#' @param conf_data A tibble with confidential data.
#' @param combos A character matrix with one row per variable combination.
#' @param weight_var Optional character name of a numeric sample-weight
#' column; when set, cell proportions are weight shares instead of row
#' shares. Defaults to `NULL` (unweighted).
#' @param na.rm A logical for dropping rows with a missing value from each
#' marginal that uses the affected variable.
#' @param allow_empty_synth A logical for permitting the synthetic data to
#' contribute no rows to a marginal (a stratum the synthesis never produced),
#' in which case its proportions are zero. A confidential marginal with no
#' rows always errors, since there is nothing to score against.
#'
#' @return A tibble with one row per cell: `variables`, `cell`,
#' `prop_synth`, `prop_conf`, and `abs_diff`.
#'
.compute_marginal_cells <- function(
  synth_data,
  conf_data,
  combos,
  weight_var = NULL,
  na.rm = FALSE,
  allow_empty_synth = FALSE
) {
  # cell proportions for one dataset over one set of variables; weighted
  # proportions are weight shares instead of row shares
  process_data <- function(data, vars, prop_name, allow_empty) {
    if (na.rm) {
      data <- dplyr::filter(
        data,
        !dplyr::if_any(.cols = dplyr::all_of(vars), .fns = is.na)
      )

      if (nrow(data) == 0 && !allow_empty) {
        stop(
          "no rows remain for the marginal over ",
          paste(vars, collapse = ", "),
          " after removing missing values"
        )
      }
    }

    if (is.null(weight_var)) {
      counts <- dplyr::count(data, dplyr::across(dplyr::all_of(vars)))
    } else {
      counts <- dplyr::count(
        data,
        dplyr::across(dplyr::all_of(vars)),
        wt = .data[[weight_var]]
      )
    }

    props <- counts |>
      dplyr::mutate("{prop_name}" := .data$n / sum(.data$n)) |>
      dplyr::select(-"n")

    return(props)
  }

  # per-cell differences for one set of variables; cells absent from one
  # dataset count as 0
  marginal_cells <- function(vars) {
    # only the synthetic side may be empty (a stratum the synthesis never
    # produced); a confidential marginal with no rows has nothing to score
    # against and errors inside process_data
    cells <- dplyr::full_join(
      process_data(
        data = synth_data, vars = vars, prop_name = "prop_synth",
        allow_empty = allow_empty_synth
      ),
      process_data(
        data = conf_data, vars = vars, prop_name = "prop_conf",
        allow_empty = FALSE
      ),
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

  cells <- purrr::map(
    .x = seq_len(nrow(combos)),
    .f = \(i) marginal_cells(vars = combos[i, ])
  ) |>
    purrr::list_rbind()

  return(cells)
}
