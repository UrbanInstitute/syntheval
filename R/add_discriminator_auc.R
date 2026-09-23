#' Add discriminator AUC to discrimination object
#'
#' @param discrimination A discrimination object with propensities (likely
#' added using add_propensities())
#' @param split A logical for if the metric should be calculated separately for
#' the training/testing split. Defaults to TRUE.
#' @param group_by_q An optional grouping variable to calculate the discriminator AUC within each group.__
#'
#' @return A discrimination object with propensities (likely added using
#' add_propensities()) with discriminator AUC. When split is TRUE or when group_by_q is
#' provided, the discriminator AUC will be calculated within each group and the returned table
#' contains a ".group" and/or ".sample" column as appropriate.
#'
#' @export
#'
add_discriminator_auc <- function(discrimination, split = TRUE, group_by_q = NULL) {

  if (split) {

    if  (!is.null(group_by_q)) {
      discriminator_auc <- discrimination$propensities |>
        dplyr::group_by(.data[[group_by_q]], .data$.sample)
    } else {
      discriminator_auc <- discrimination$propensities |>
        dplyr::group_by(.data$.sample)
    }

    discriminator_auc <- discriminator_auc |>
      yardstick::roc_auc(".source_label", ".pred_synthetic")

    if (!is.null(group_by_q)) {
      discriminator_auc <- discriminator_auc |>
        dplyr::rename(".group" := !!rlang::sym(group_by_q))
    }

    discriminator_auc <- discriminator_auc |>
      dplyr::mutate(.sample = factor(.data$.sample, levels = c("training", "testing"))) |>
      dplyr::arrange(.data$.sample) |>
      dplyr::ungroup()

  } else {

    if (!is.null(group_by_q)) {
      discriminator_auc <- discrimination$propensities |>
        dplyr::group_by(.data[[group_by_q]])
    } else {
      discriminator_auc <- discrimination$propensities
    }

    discriminator_auc <- discriminator_auc |>
      yardstick::roc_auc(".source_label", ".pred_synthetic")

    if (!is.null(group_by_q)) {
      discriminator_auc <- discriminator_auc |>
        dplyr::rename(".group" := !!rlang::sym(group_by_q))
    }

    discriminator_auc <- discriminator_auc |>
      dplyr::mutate(.sample = factor("overall", levels = "overall"))

  }

  discrimination$discriminator_auc <- discriminator_auc

  return(discrimination)

}
