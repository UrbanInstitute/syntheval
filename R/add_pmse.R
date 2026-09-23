#' Add pMSE to discrimination object
#'
#' @param discrimination A discrimination object with propensities (likely
#' added using add_propensities())
#' @param split A logical for if the metric should be calculated separately for
#' the training/testing split. Defaults to TRUE.
#' @param group_by_q An optional grouping variable to calculate the pMSE within each group.
#'
#' @return A discrimination object with propensities (likely added using
#' add_propensities()) with a pMSE. When split is TRUE or when group_by_q is
#' provided, the pMSEwill be calculated within each group and the returned table
#' contains a ".group" and/or ".sample" column as appropriate.
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
      dplyr::ungroup() |>
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
        list(
          discrimination$propensities[[group_by_q]],
          discrimination$propensities$.sample
        ),
        drop = FALSE
      )
      pmse_list <- lapply(propensities_by, calc_pmse)

      # Get unique group and sample values in order
      groups <- unique(discrimination$propensities[[group_by_q]])
      samples <- c("training", "testing")

      # Create all combinations and look up pmse values
      pmse_data <- expand.grid(.group = groups, .source = samples, stringsAsFactors = FALSE)
      pmse_data$.source <- factor(pmse_data$.source, levels = c("training", "testing"))

      # Create keys to match with pmse_list
      pmse_data$.key <- paste(pmse_data$.group, pmse_data$.source, sep = ".")
      pmse_data$.pmse <- sapply(pmse_data$.key, function(k) pmse_list[[k]], USE.NAMES = FALSE)
      pmse_data$.key <- NULL

      # Convert .group to factor
      pmse_data$.group <- factor(pmse_data$.group)

      pmse <- tibble::as_tibble(pmse_data)

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
        discrimination$propensities[[group_by_q]],
        drop = FALSE
      )
      pmse_list <- lapply(propensities_by, calc_pmse)

      pmse <- tibble::tibble(
        .group = factor(names(pmse_list)),
        .source = factor("overall", levels = "overall"),
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
