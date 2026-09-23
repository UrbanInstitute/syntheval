#' Add pMSE ratio to discrimination object
#'
#' `method = "perm"` estimates the null pMSEs in the denominator of the ratio
#' by randomly permuting the confidential/synthetic labels, which can be computed
#' in parallel. The permutation iterations are evaluated with
#' `furrr::future_map()`, so they run sequentially by default and in parallel
#' under a non-sequential [future::plan()] (for example,
#' `future::plan(future::multisession)`). Results are reproducible for a given
#' seed and identical under sequential and parallel plans.
#'
#' `method = "logistic"` instead calculates the null pMSE with the closed-form
#' approximation for logistic regression discriminators from Snoke et al.
#' (2018): \eqn{(k - 1)(1 - c)^2 / N}, where \eqn{k} is the number of
#' coefficients in the fitted model (including the intercept), \eqn{c} is the
#' proportion of synthetic records, and \eqn{N} is the number of records. This
#' requires the discriminator to be a `parsnip::logistic_reg()` model with the
#' `"glm"` engine and does not require `times`.
#'
#' @param discrimination A discrimination with added propensities
#' @param split A logical for if the metric should be calculated separately for
#' the training/testing split. Defaults to TRUE.
#' @param prop The proportion of data to be retained for modeling/analysis in
#' the training/testing split. The sampling is stratified by the confidential and
#' synthetic data.
#' @param times The number of permutations. Only used when `method = "perm"`.
#' @param method The method used to estimate the null pMSE. `"perm"` (the
#' default) permutes the confidential/synthetic labels. `"logistic"` uses the
#' closed-form approximation for logistic regression discriminators from
#' Snoke et al. (2018).
#' @param group_by_q An optional grouping variable to calculate the pMSE ratio within each group.
#'
#' @return A discrimination with pMSE. When split is TRUE or when group_by_q is
#' provided, the pMSE ratio will be calculated within each group and the returned table
#' contains a ".group" and/or ".sample" column as appropriate.
#'
#' @family Utility metrics
#'
#' @export
add_pmse_ratio <- function(discrimination, split = TRUE, prop = 4 / 5, times = NULL, method = "perm", group_by_q = NULL) {

  method <- match.arg(method, choices = c("perm", "logistic"))

  if (method == "perm" && (is.null(times) || times %% 1 != 0 || times < 1)) {

    stop('Error: times must be a positive integer when method is "perm"')

  }

  if (is.null(discrimination$pmse)) {

    stop("Error: discrimination must have a pmse. Use add_pmse() before add_pmse_ratio()")

  }

  if (method == "logistic") {

    return(.add_pmse_ratio_logistic(discrimination))

  }

  calc_pmse <- function(propensities, group_by_q = NULL) {

    # calculate the expected propensity
    if (!is.null(group_by_q)) {
      prop_synthetic <- propensities |>
        dplyr::ungroup() |>
        dplyr::group_by(dplyr::across(dplyr::all_of(group_by_q))) |>
        dplyr::summarize(
          prop_synthetic = sum(.data$.source_label == "synthetic") / dplyr::n(),
          .groups = "drop"
        ) |>
        dplyr::pull("prop_synthetic")
    } else {
      prop_synthetic <- sum(propensities$.source_label == "synthetic") / nrow(propensities)
    }

    propensities_vec <- propensities |>
      dplyr::pull(".pred_synthetic")

    # calculate the observed pMSE
    pmse <- mean((propensities_vec - prop_synthetic) ^ 2)

    return(pmse)

  }
  # calculate the null pMSE for one permutation
  #
  # param: iteration_index The permutation iteration number. Unused because
  # each iteration draws a fresh label permutation from its own random seed.
  #
  # return: A named list with the overall, training, and testing null pMSEs.
  # The training and testing elements are NA when split = FALSE.
  calc_null_pmse <- function(iteration_index) {

    # shuffle the confidential/synthetic labels so they carry no information,
    # keeping every other column and the label proportions unchanged
    permuted_sample <- discrimination$combined_data |>
      dplyr::mutate(.source_label = sample(.data$.source_label))

    if (split) {

      # make training/testing split
      data_split <- rsample::initial_split(
        data = permuted_sample,
        prop = prop,
        strata = ".source_label"
      )

      # fit the model from the pMSE on the permuted sample
      fitted_model <- parsnip::fit(
        discrimination$discriminator,
        data = rsample::training(data_split)
      )

      # calculate the propensities
      propensities_df <- dplyr::bind_cols(
        stats::predict(fitted_model, new_data = discrimination$combined_data, type = "prob")[, ".pred_synthetic"],
        discrimination$combined_data
      ) |>
        dplyr::mutate(
          .sample = dplyr::if_else(
            dplyr::row_number() %in% data_split$in_id,
            true = "training",
            false = "testing"
          )
        )

      # calculate the pmse for each permutation
      pmse_null <- list(
        overall = calc_pmse(propensities_df, group_by_q = group_by_q),
        training = propensities_df |>
          dplyr::filter(.data$.sample == "training") |>
          calc_pmse(group_by_q = group_by_q),
        testing = propensities_df |>
          dplyr::filter(.data$.sample == "testing") |>
          calc_pmse(group_by_q = group_by_q)
      )

    } else {

      # fit the model from the pMSE on the permuted sample
      fitted_model <- parsnip::fit(
        discrimination$discriminator,
        data = permuted_sample
      )

      # calculate the propensities
      propensities_df <- dplyr::bind_cols(
        stats::predict(fitted_model, new_data = discrimination$combined_data, type = "prob")[, ".pred_synthetic"],
        discrimination$combined_data
      )

      # calculate the pmse for each permutation
      pmse_null <- list(
        overall = calc_pmse(propensities_df, group_by_q = group_by_q),
        training = NA_real_,
        testing = NA_real_
      )

    }

    return(pmse_null)

  }

  # calculate the null pMSE for each permutation, in parallel when a
  # non-sequential future::plan() is set
  # seed = TRUE assigns every iteration its own random seed up front, so
  # results are reproducible and identical across plans for a given seed;
  # packages = "workflows" loads the fit.workflow method on parallel workers
  pmse_null <- furrr::future_map(
    .x = seq_len(times),
    .f = calc_null_pmse,
    .options = furrr::furrr_options(seed = TRUE, packages = "workflows")
  )

  # find the mean of the permuted pMSEs
  mean_null_pmse_overall <- mean(purrr::map_dbl(pmse_null, "overall"))
  mean_null_pmse_training <- mean(purrr::map_dbl(pmse_null, "training"))
  mean_null_pmse_testing <- mean(purrr::map_dbl(pmse_null, "testing"))

  # calculate the ratio for the training/testing split or overall data
  if (all(c("training", "testing") %in% discrimination$pmse$.source)) {

    if (!split) {

      stop(
        "Error: discrimination$pmse contains training/testing pMSEs but ",
        "split = FALSE, so no training/testing null pMSEs were permuted. ",
        "Call add_pmse_ratio() with split = TRUE."
      )

    }

    # Check if there's grouping in the pmse
    if (".group" %in% names(discrimination$pmse)) {
      
      # Create null pMSE values for each group-split combination
      null_pmse_vec <- discrimination$pmse |>
        dplyr::mutate(
          .null_pmse = dplyr::if_else(
            .data$.source == "training",
            mean_null_pmse_training,
            mean_null_pmse_testing
          )
        ) |>
        dplyr::pull(".null_pmse")
      
      pmse <- dplyr::bind_cols(
        discrimination$pmse,
        tibble::tibble(.null_pmse = null_pmse_vec)
      ) |>
        dplyr::mutate(.pmse_ratio = .data$.pmse / .data$.null_pmse)
      
    } else {
      
      pmse <- dplyr::bind_cols(
        discrimination$pmse,
        tibble::tibble(.null_pmse = c(mean_null_pmse_training, mean_null_pmse_testing))
      ) |>
        dplyr::mutate(.pmse_ratio = .data$.pmse / .data$.null_pmse)
      
    }

  } else {

    pmse <- dplyr::bind_cols(
      discrimination$pmse,
      tibble::tibble(.null_pmse = mean_null_pmse_overall)
    ) |>
      dplyr::mutate(.pmse_ratio = .data$.pmse / .data$.null_pmse)

  }

  discrimination$pmse <- pmse

  return(discrimination)

}

# calculate the null pMSE with the closed-form logistic regression
# approximation from Snoke et al. (2018): (k - 1)(1 - c)^2 / N
.add_pmse_ratio_logistic <- function(discrimination) {

  spec <- workflows::extract_spec_parsnip(discrimination$discriminator)

  if (!inherits(spec, "logistic_reg") || spec$engine != "glm") {

    stop(
      "Error: method = \"logistic\" requires the discriminator to be a ",
      "parsnip::logistic_reg() model with the \"glm\" engine."
    )

  }

  # number of coefficients in the fitted model, including the intercept
  k <- length(stats::coef(workflows::extract_fit_engine(discrimination$discriminator)))

  # null pMSE for one subset of propensities (training, testing, or overall)
  calc_null_pmse <- function(propensities) {

    n <- nrow(propensities)
    c <- mean(propensities$.source_label == "synthetic")

    (k - 1) * (1 - c) ^ 2 / n

  }

  if (all(c("training", "testing") %in% discrimination$pmse$.source)) {

    null_pmse_training <- discrimination$propensities |>
      dplyr::filter(.data$.sample == "training") |>
      calc_null_pmse()

    null_pmse_testing <- discrimination$propensities |>
      dplyr::filter(.data$.sample == "testing") |>
      calc_null_pmse()

    # Check if there's grouping in the pmse
    if (".group" %in% names(discrimination$pmse)) {
      
      # Replicate null pMSE for each group-split combination
      pmse <- discrimination$pmse |>
        dplyr::mutate(
          .null_pmse = dplyr::if_else(
            .data$.source == "training",
            null_pmse_training,
            null_pmse_testing
          )
        ) |>
        dplyr::mutate(.pmse_ratio = .data$.pmse / .data$.null_pmse)
      
    } else {
      
      pmse <- dplyr::bind_cols(
        discrimination$pmse,
        tibble::tibble(.null_pmse = c(null_pmse_training, null_pmse_testing))
      ) |>
        dplyr::mutate(.pmse_ratio = .data$.pmse / .data$.null_pmse)
      
    }

  } else {

    null_pmse_overall <- calc_null_pmse(discrimination$propensities)

    pmse <- dplyr::bind_cols(
      discrimination$pmse,
      tibble::tibble(.null_pmse = null_pmse_overall)
    ) |>
      dplyr::mutate(.pmse_ratio = .data$.pmse / .data$.null_pmse)

  }

  discrimination$pmse <- pmse

  return(discrimination)

}
