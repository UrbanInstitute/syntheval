#' @title Compute stratified k-marginals results
#'
#' @description Splits the synthetic and confidential data into the strata
#' observed in the confidential data, computes per-cell differences and
#' per-combination MabsDDs within each stratum, and rolls the per-stratum
#' scores up weighted by each stratum's confidential share.
#'
#' @param synth_data A tibble with synthetic data.
#' @param conf_data A tibble with confidential data.
#' @param combos A character matrix with one row per variable combination.
#' @param group_by Character vector of grouping variable names.
#' @param weight_var Optional character name of a numeric sample-weight
#' column; when set, cell proportions and stratum shares are weight shares
#' instead of row shares. Defaults to `NULL` (unweighted).
#' @param na.rm A logical for dropping rows with a missing value from each
#' marginal that uses the affected variable.
#'
#' @return A list with `score` (the share-weighted mean of per-stratum
#' mean(MabsDD) scores on the 0 to 1 scale), `marginals` and `cells` (stacked
#' per-stratum tables with the grouping columns, worst first), and
#' `group_scores` (one row per stratum with its share and score, worst
#' first).
#'
.stratify_k_marginals <- function(
  synth_data,
  conf_data,
  combos,
  group_by,
  weight_var = NULL,
  na.rm = FALSE
) {
  # strata are defined by the confidential data; a stratum with no synthetic
  # rows scores against all-zero synthetic proportions. Shares are the
  # confidential row (or weight) share of each stratum, computed once
  conf_totals <- if (is.null(weight_var)) {
    rep(1, nrow(conf_data))
  } else {
    conf_data[[weight_var]]
  }

  strata <- conf_data |>
    dplyr::mutate(.stratum_total = conf_totals) |>
    dplyr::summarize(
      .share = sum(.data$.stratum_total),
      .by = dplyr::all_of(group_by)
    ) |>
    dplyr::mutate(.share = .data$.share / sum(.data$.share))

  per_stratum <- purrr::map(
    .x = seq_len(nrow(strata)),
    .f = \(i) {
      stratum <- strata[i, group_by, drop = FALSE]

      synth_g <- dplyr::semi_join(synth_data, stratum, by = group_by)
      conf_g <- dplyr::semi_join(conf_data, stratum, by = group_by)

      cells_g <- .compute_marginal_cells(
        synth_data = synth_g,
        conf_data = conf_g,
        combos = combos,
        weight_var = weight_var,
        na.rm = na.rm,
        allow_empty_synth = TRUE
      )

      marginals_g <- cells_g |>
        dplyr::summarize(madd = mean(.data$abs_diff), .by = "variables")

      list(
        cells = dplyr::bind_cols(stratum, cells_g),
        marginals = dplyr::bind_cols(stratum, marginals_g),
        group_scores = dplyr::bind_cols(
          stratum,
          tibble::tibble(
            share = strata$.share[i],
            score = mean(marginals_g$madd)
          )
        )
      )
    }
  )

  cells <- purrr::list_rbind(purrr::map(per_stratum, "cells")) |>
    dplyr::arrange(dplyr::desc(.data$abs_diff))

  marginals <- purrr::list_rbind(purrr::map(per_stratum, "marginals")) |>
    dplyr::arrange(dplyr::desc(.data$madd))

  group_scores <- purrr::list_rbind(
    purrr::map(per_stratum, "group_scores")
  ) |>
    dplyr::arrange(dplyr::desc(.data$score))

  # per-stratum scores roll up weighted by confidential shares, so small
  # strata surface in group_scores without dominating the headline
  score <- sum(group_scores$share * group_scores$score)

  return(
    list(
      score = score,
      marginals = marginals,
      cells = cells,
      group_scores = group_scores
    )
  )
}
