#' Add pMSE (propensity score mean squared error) to discrimination object
#'
#' @param discrimination A discrimination object with propensities (likely
#' added using add_propensities())
#' @param split A logical for if the metric should be calculated separately for
#' the training/testing split. Defaults to TRUE.
#' @param group_by_q An optional grouping variable to calculate the pMSE within each group.
#'
#' @return A discrimination object with propensities (likely added using
#' add_propensities()) with a pMSE
#'
#' @family Utility metrics
#'
#' @export
#'
add_pmse <- function(discrimination, split = TRUE, group_by_q = NULL) {

  calc_pmse <- function(propensities) {

    # calculate the expected propensity
    prop_synthetic <- propensities |>
      dplyr::summarize(
        n_synthetic = sum(.data$.source_label == "synthetic"),
        n_total = dplyr::n()
      ) |>
      dplyr::mutate(prop_synthetic = .data$n_synthetic / .data$n_total) |>
      dplyr::pull("prop_synthetic")

    propensities_vec <- propensities |>
      dplyr::pull(".pred_synthetic")

    # calculate the observed pMSE
    pmse <- mean((propensities_vec - prop_synthetic) ^ 2)

    return(pmse)

  }

  if (split) {

    if (!is.null(group_by_q)) {

      propensities_by <- split(
        discrimination$propensities,
        c(
          discrimination$propensities[[group_by_q]],
          discrimination$propensities$.sample
        )
      )
      pmse_list <- lapply(propensities_by, calc_pmse)
      pmse <- tibble::tibble(
        .source = factor(names(pmse_list), levels = names(pmse_list)),
        .pmse = unlist(pmse_list)
      )

    } else {

      # create list of dataframes by split (training/testing)
      propensities_by <- split(discrimination$propensities, discrimination$propensities$.sample)
      pmse_list <- lapply(propensities_by, calc_pmse)

      pmse <- tibble::tibble(
        .source = factor(c("training", "testing"), levels = c("training", "testing")),
        .pmse = c(pmse_list$training, pmse_list$testing)
      )

    }

  } else {

    if (!is.null(group_by_q)) {

      propensities_by <- split(
        discrimination$propensities,
        discrimination$propensities[[group_by_q]]
      )
      pmse_list <- lapply(propensities_by, calc_pmse)
      pmse <- tibble::tibble(
        .source = factor(names(pmse_list), levels = names(pmse_list)),
        .pmse = unlist(pmse_list)
      )

    } else {

      pmse_overall <- discrimination$propensities |>
      calc_pmse()

      pmse <- tibble::tibble(
        .source = factor("overall", levels = "overall"),
        .pmse = pmse_overall
      )

    }


  }

  discrimination$pmse <- pmse

  return(discrimination)

}