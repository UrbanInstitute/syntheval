#' Fit models and generate predictions comparing confidential and synthetic data
#'
#' Fits the supplied `workflow` on the confidential data and on the synthetic
#' data, then generates predictions from both fitted models. Dispatches on
#' whether `eval_data` contains `holdout_data`: if present, both fitted models
#' generate predictions on the holdout data, which is the strongest assessment
#' of predictive utility because the holdout data are never used for
#' synthesis or model fitting/tuning. Currently only this holdout procedure is
#' supported; a future iteration will add a no-holdout procedure.
#'
#' @param eval_data An `eval_data` object.
#' @param workflow An unfitted `workflow` object from the `workflows` package.
#'
#' @return A `pred` object, a list with:
#'   * `procedure`: `"holdout"`.
#'   * `models`: a list with the fitted `confidential` and `synthetic` workflows.
#'   * `predictions`: a tibble of predictions from each fitted model, with a
#'     `source` column (`"confidential"` or `"synthetic"`), prediction columns
#'     from `predict()`, and the evaluation data columns (including the
#'     outcome).
#'
#' @family Predictive utility metrics
#'
#' @export
#'
util_pred <- function(eval_data, workflow) {

  stopifnot(is_eval_data(eval_data))

  if (is.null(eval_data$holdout_data)) {

    stop(
      "util_pred() currently only supports eval_data objects with holdout_data.",
      call. = FALSE
    )

  }

  stopifnot(inherits(workflow, "workflow"))
  stopifnot(eval_data$n_rep == 1)

  conf_model <- parsnip::fit(workflow, data = eval_data$conf_data)
  synth_model <- parsnip::fit(workflow, data = eval_data$synth_data)

  conf_predictions <- dplyr::bind_cols(
    source = "confidential",
    stats::predict(conf_model, new_data = eval_data$holdout_data),
    eval_data$holdout_data
  )

  synth_predictions <- dplyr::bind_cols(
    source = "synthetic",
    stats::predict(synth_model, new_data = eval_data$holdout_data),
    eval_data$holdout_data
  )

  pred <- list(
    procedure = "holdout",
    models = list(confidential = conf_model, synthetic = synth_model),
    predictions = dplyr::bind_rows(conf_predictions, synth_predictions)
  )

  structure(pred, class = "pred")

}
