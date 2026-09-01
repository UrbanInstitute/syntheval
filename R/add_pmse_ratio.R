#' Add pMSE ratio to discrimination object
#'
#' The null pMSEs in the denominator of the ratio are estimated with bootstrap
#' samples that can be computed in parallel. The bootstrap iterations are
#' evaluated with `furrr::future_map()`, so they run sequentially by default
#' and in parallel under a non-sequential [future::plan()] (for example,
#' `future::plan(future::multisession)`). Results are reproducible for a given
#' seed and identical under sequential and parallel plans.
#'
#' @param discrimination A discrimination with added propensities
#' @param split A logical for if the metric should be calculated separately for
#' the training/testing split. Defaults to TRUE.
#' @param prop The proportion of data to be retained for modeling/analysis in
#' the training/testing split. The sampling is stratified by the original and
#' synthetic data.
#' @param times The number of bootstrap samples.
#'
#' @return A discrimination with pMSE
#'
#' @family Utility metrics
#'
#' @export
add_pmse_ratio <- function(discrimination, split = TRUE, prop = 3 / 4, times) {

  if (is.null(discrimination$pmse)) {

    stop("Error: discrimination must have a pmse. Use add_pmse() before add_pmse_ratio()")

  }

  calc_pmse <- function(propensities) {

    # calculate the expected propensity
    prop_synthetic <- propensities %>%
      dplyr::summarize(
        n_synthetic = sum(.data$.source_label == "synthetic"),
        n_total = dplyr::n()
      ) %>%
      dplyr::mutate(prop_synthetic = .data$n_synthetic / .data$n_total) %>%
      dplyr::pull("prop_synthetic")

    propensities_vec <- propensities %>%
      dplyr::pull(".pred_synthetic")

    # calculate the observed pMSE
    pmse <- mean((propensities_vec - prop_synthetic) ^ 2)

    return(pmse)

  }
  # calculate the null pMSE for one bootstrap sample
  #
  # param: iteration_index The bootstrap iteration number. Unused because each
  # iteration draws a fresh bootstrap sample from its own random seed.
  #
  # return: A named list with the overall, training, and testing null pMSEs.
  # The training and testing elements are NA when split = FALSE.
  calc_null_pmse <- function(iteration_index) {

    # bootstrap sample original observations to equal the size of the combined
    # data
    # append the original labels so the proportions match
    bootstrap_sample <- dplyr::bind_cols(
      discrimination$combined_data %>%
        dplyr::filter(.data$.source_label == "original") %>%
        dplyr::slice_sample(n = nrow(discrimination$combined_data), replace = TRUE) %>%
        dplyr::select(-".source_label"),
      discrimination$combined_data %>%
        dplyr::select(".source_label")
    )

    if (split) {

      # make training/testing split
      data_split <- rsample::initial_split(
        data = bootstrap_sample,
        prop = prop,
        strata = ".source_label"
      )

      # fit the model from the pMSE on the bootstrap sample
      fitted_model <- parsnip::fit(
        discrimination$discriminator,
        data = rsample::training(data_split)
      )

      # calculate the propensities
      propensities_df <- dplyr::bind_cols(
        stats::predict(fitted_model, new_data = discrimination$combined_data, type = "prob")[, ".pred_synthetic"],
        discrimination$combined_data
      ) %>%
        dplyr::mutate(
          .sample = dplyr::if_else(
            dplyr::row_number() %in% data_split$in_id,
            true = "training",
            false = "testing"
          )
        )

      # calculate the pmse for each bootstrap
      pmse_null <- list(
        overall = calc_pmse(propensities_df),
        training = propensities_df %>%
          dplyr::filter(.data$.sample == "training") %>%
          calc_pmse(),
        testing = propensities_df %>%
          dplyr::filter(.data$.sample == "testing") %>%
          calc_pmse()
      )

    } else {

      # fit the model from the pMSE on the bootstrap sample
      fitted_model <- parsnip::fit(
        discrimination$discriminator,
        data = bootstrap_sample
      )

      # calculate the propensities
      propensities_df <- dplyr::bind_cols(
        stats::predict(fitted_model, new_data = discrimination$combined_data, type = "prob")[, ".pred_synthetic"],
        discrimination$combined_data
      )

      # calculate the pmse for each bootstrap
      pmse_null <- list(
        overall = calc_pmse(propensities_df),
        training = NA_real_,
        testing = NA_real_
      )

    }

    return(pmse_null)

  }

  # calculate the null pMSE for each bootstrap sample, in parallel when a
  # non-sequential future::plan() is set
  # seed = TRUE assigns every iteration its own random seed up front, so
  # results are reproducible and identical across plans for a given seed;
  # packages = "workflows" loads the fit.workflow method on parallel workers
  pmse_null <- furrr::future_map(
    .x = seq_len(times),
    .f = calc_null_pmse,
    .options = furrr::furrr_options(seed = TRUE, packages = "workflows")
  )

  # find the mean of the bootstrapped pMSEs
  mean_null_pmse_overall <- mean(purrr::map_dbl(pmse_null, "overall"))
  mean_null_pmse_training <- mean(purrr::map_dbl(pmse_null, "training"))
  mean_null_pmse_testing <- mean(purrr::map_dbl(pmse_null, "testing"))

  # calculate the ratio for the training/testing split or overall data
  if (all(c("training", "testing") %in% discrimination$pmse$.source)) {

    if (!split) {

      stop(
        "Error: discrimination$pmse contains training/testing pMSEs but ",
        "split = FALSE, so no training/testing null pMSEs were bootstrapped. ",
        "Call add_pmse_ratio() with split = TRUE."
      )

    }

    pmse <- dplyr::bind_cols(
      discrimination$pmse,
      tibble::tibble(.null_pmse = c(mean_null_pmse_training, mean_null_pmse_testing))
    ) %>%
      dplyr::mutate(.pmse_ratio = .data$.pmse / .data$.null_pmse)

  } else {

    pmse <- dplyr::bind_cols(
      discrimination$pmse,
      tibble::tibble(.null_pmse = mean_null_pmse_overall)
    ) %>%
      dplyr::mutate(.pmse_ratio = .data$.pmse / .data$.null_pmse)

  }

  discrimination$pmse <- pmse

  return(discrimination)

}
