#' Add SPECKS to discrimination object
#'
#' @param discrimination A discrimination with added propensities
#' @param split A logical for if the metric should be calculated separately for 
#' the training/testing split. Defaults to TRUE.
#' @param group_by_q An optional grouping variable to calculate SPECKS within each group.
#'
#' @family Utility metrics
#'
#' @return A discrimination with SPECKS
#'
#' @export
add_specks <- function(discrimination, split = TRUE, group_by_q = NULL) {

  calc_specks <- function(propensities) {

    propensities_original <- propensities |>
      dplyr::filter(.data$.source_label == "original") |>
      dplyr::pull(".pred_synthetic")

    propensities_synthetic <- propensities |>
      dplyr::filter(.data$.source_label == "synthetic") |>
      dplyr::pull(".pred_synthetic")

    # Calculate KS Distance of the original and synthetic ECDFS
    specks <- suppressWarnings(
      stats::ks.test(
        propensities_original,
        propensities_synthetic,
        exact = FALSE)$statistic
    )

    specks <- unname(specks)

    return(specks)

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
      specks_list <- lapply(propensities_by, calc_specks)

      # Get unique group and sample values in order
      groups <- unique(discrimination$propensities[[group_by_q]])
      samples <- c("training", "testing")
      
      # Create all combinations and look up specks values
      specks_data <- expand.grid(.group = groups, .source = samples, stringsAsFactors = FALSE)
      specks_data$.source <- factor(specks_data$.source, levels = c("training", "testing"))
      
      # Create keys to match with specks_list
      specks_data$.key <- paste(specks_data$.group, specks_data$.source, sep = ".")
      specks_data$.specks <- sapply(specks_data$.key, function(k) specks_list[[k]], USE.NAMES = FALSE)
      specks_data$.key <- NULL
      
      # Convert .group to factor
      specks_data$.group <- factor(specks_data$.group)
      
      specks <- tibble::as_tibble(specks_data)

    } else {

      specks_training <- discrimination$propensities |>
        dplyr::filter(.data$.sample == "training") |>
        calc_specks()

      specks_testing <- discrimination$propensities |>
        dplyr::filter(.data$.sample == "testing") |>
        calc_specks()

      specks <- tibble::tibble(
        .source = factor(c("training", "testing"), levels = c("training", "testing")),
        .specks = c(specks_training, specks_testing)
      )

    }

  } else {

    if (!is.null(group_by_q)) {

      propensities_by <- split(
        discrimination$propensities,
        discrimination$propensities[[group_by_q]],
        drop = FALSE
      )
      specks_list <- lapply(propensities_by, calc_specks)
      
      specks <- tibble::tibble(
        .group = factor(names(specks_list)),
        .source = factor("overall", levels = "overall"),
        .specks = unlist(specks_list)
      )

    } else {

      specks_overall <- discrimination$propensities |>
        calc_specks()

      specks <- tibble::tibble(
        .source = factor("overall", levels = "overall"),
        .specks = specks_overall
      )

    }

  }

  discrimination$specks <- specks

  return(discrimination)

}

