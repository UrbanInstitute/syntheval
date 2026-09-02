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
#' @param equalize_data a logical evaluating to TRUE or FALSE indicating whether
#' the number of rows in the data should be equalized 
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

  stopifnot(inherits(workflow, "workflow"))
  # multiple synthetic replicates aren't supported yet
  stopifnot(eval_data$n_rep == 1)
  
  if (is.null(eval_data$holdout_data)) {

    conf_data <- eval_data$conf_data
    synth_data <- eval_data$synth_data
    
    # add option to equalize the size of the confidential and synthetic data
    if (equalize_data) {
    
      min_n <- min(nrow(conf_data), nrow(synth_data))
      
      conf_data <- dplyr::slice_sample(eval_data$conf_data, n = min_n)
      synth_data <- dplyr::slice_sample(eval_data$synth_data, n = min_n)
      
    }
    
    # split the confidential and synthetic data into training (called modeling)
    # and testing (called implementation)
    conf_split <- rsample::initial_split(conf_data, prop = 0.8)
    synth_split <- rsample::initial_split(synth_data, prop = 0.8)
    
    conf_modeling <- rsample::training(conf_split)
    conf_implementation <- rsample::testing(conf_split)
    synth_modeling <- rsample::training(synth_split)
    synth_implementation <- rsample::testing(synth_split)
    
    # fit the workflows
    conf_model <- parsnip::fit(workflow, data = conf_modeling)
    synth_model <- parsnip::fit(workflow, data = synth_modeling)
  
    # apply the workflows to the four data sets
    
    # class probabilities are only defined for classification models, and
    # pred_auc() needs them, so fetch them when available
    if (workflows::extract_spec_parsnip(workflow)$mode == "classification") {
      
      conf_modeling_probs <- stats::predict(conf_model, new_data = conf_modeling, type = "prob")
      conf_implementation_probs <- stats::predict(conf_model, new_data = conf_implementation, type = "prob")
      synth_modeling_probs <- stats::predict(conf_model, new_data = synth_modeling, type = "prob")
      synth_implementation_probs <- stats::predict(conf_model, new_data = synth_implementation, type = "prob")

    } else {
      
      conf_modeling_probs <- NULL
      conf_implementation_probs <- NULL
      synth_modeling_probs <- NULL
      synth_implementation_probs <- NULL
    
    }
    
    conf_modeling_predictions <- dplyr::bind_cols(
      source = "confidential modeling",
      stats::predict(conf_model, new_data = conf_modeling),
      conf_modeling_probs,
      conf_modeling
    )
    
    conf_implementation_predictions <- dplyr::bind_cols(
      source = "confidential implementation",
      stats::predict(conf_model, new_data = conf_implementation),
      conf_implementation_probs,
      conf_implementation
    )
    
    synth_modeling_predictions <- dplyr::bind_cols(
      source = "synthetic modeling",
      stats::predict(synth_model, new_data = synth_modeling),
      synth_modeling_probs,
      synth_modeling
    )
    
    synth_implementation_predictions <- dplyr::bind_cols(
      source = "synthetic implementation",
      stats::predict(synth_model, new_data = synth_implementation),
      synth_implementation_probs,
      synth_implementation
    )
    
    pred <- list(
      procedure = "split",
      # retained for future variable importance comparisons
      models = list(
        confidential = conf_model, 
        synthetic = synth_model
      ),
      predictions = dplyr::bind_rows(
        conf_modeling_predictions, 
        conf_implementation_predictions,
        synth_modeling_predictions,
        synth_implementation_predictions
      )
    )
    
  } else {
    
    # the confidential and synthetic models must be fit separately so their
    # predictions on the holdout data can be compared
    conf_model <- parsnip::fit(workflow, data = eval_data$conf_data)
    synth_model <- parsnip::fit(workflow, data = eval_data$synth_data)
    
    # class probabilities are only defined for classification models, and
    # pred_auc() needs them, so fetch them when available
    if (workflows::extract_spec_parsnip(workflow)$mode == "classification") {
      
      conf_probs <- stats::predict(conf_model, new_data = eval_data$holdout_data, type = "prob")
      synth_probs <- stats::predict(synth_model, new_data = eval_data$holdout_data, type = "prob")
      
    } else {
      
      conf_probs <- NULL
      synth_probs <- NULL
      
    }
    
    # keep the holdout outcome and covariates alongside predictions so
    # pred_*() functions can compute metrics without needing eval_data again
    conf_predictions <- dplyr::bind_cols(
      source = "confidential",
      stats::predict(conf_model, new_data = eval_data$holdout_data),
      conf_probs,
      eval_data$holdout_data
    )
    
    synth_predictions <- dplyr::bind_cols(
      source = "synthetic",
      stats::predict(synth_model, new_data = eval_data$holdout_data),
      synth_probs,
      eval_data$holdout_data
    )
    
    pred <- list(
      procedure = "holdout",
      # retained for future variable importance comparisons
      models = list(confidential = conf_model, synthetic = synth_model),
      predictions = dplyr::bind_rows(conf_predictions, synth_predictions)
    )
    
  }
  
  structure(pred, class = "pred")

}

# the outcome variable name isn't stored on pred directly, so pred_*()
# functions look it up from the fitted workflow's mold
.pred_outcome_name <- function(pred) {
  names(workflows::extract_mold(pred$models$confidential)$outcomes)
}

#' Area under the ROC curve for a `pred` object
#'
#' @param pred A `pred` object created by `util_pred()`.
#'
#' @return A tibble with one row per data source (`confidential`/`synthetic`)
#' and the ROC AUC for predictions from that source's model.
#'
#' @family Predictive utility metrics
#'
#' @export
#'
pred_auc <- function(pred) {

  stopifnot(inherits(pred, "pred"))

  outcome <- .pred_outcome_name(pred)
  event_level <- levels(pred$predictions[[outcome]])[1]
  prob_col <- paste0(".pred_", event_level)

  pred$predictions |>
    dplyr::group_by(.data$source) |>
    yardstick::roc_auc(truth = !!rlang::sym(outcome), !!rlang::sym(prob_col)) |>
    dplyr::ungroup()

}

#' Precision for a `pred` object
#'
#' @param pred A `pred` object created by `util_pred()`.
#'
#' @return A tibble with one row per data source (`confidential`/`synthetic`)
#' and the precision of predictions from that source's model.
#'
#' @family Predictive utility metrics
#'
#' @export
#'
pred_precision <- function(pred) {

  stopifnot(inherits(pred, "pred"))

  outcome <- .pred_outcome_name(pred)

  pred$predictions |>
    dplyr::group_by(.data$source) |>
    yardstick::precision(truth = !!rlang::sym(outcome), estimate = .pred_class) |>
    dplyr::ungroup()

}

#' Recall for a `pred` object
#'
#' @param pred A `pred` object created by `util_pred()`.
#'
#' @return A tibble with one row per data source (`confidential`/`synthetic`)
#' and the recall of predictions from that source's model.
#'
#' @family Predictive utility metrics
#'
#' @export
#'
pred_recall <- function(pred) {

  stopifnot(inherits(pred, "pred"))

  outcome <- .pred_outcome_name(pred)

  pred$predictions |>
    dplyr::group_by(.data$source) |>
    yardstick::recall(truth = !!rlang::sym(outcome), estimate = .pred_class) |>
    dplyr::ungroup()

}

#' Root mean squared error for a `pred` object
#'
#' @param pred A `pred` object created by `util_pred()`.
#'
#' @return A tibble with one row per data source (`confidential`/`synthetic`)
#' and the RMSE of predictions from that source's model.
#'
#' @family Predictive utility metrics
#'
#' @export
#'
pred_rmse <- function(pred) {

  stopifnot(inherits(pred, "pred"))

  outcome <- .pred_outcome_name(pred)

  pred$predictions |>
    dplyr::group_by(.data$source) |>
    yardstick::rmse(truth = !!rlang::sym(outcome), estimate = .pred) |>
    dplyr::ungroup()

}

#' Mean absolute error for a `pred` object
#'
#' @param pred A `pred` object created by `util_pred()`.
#'
#' @return A tibble with one row per data source (`confidential`/`synthetic`)
#' and the MAE of predictions from that source's model.
#'
#' @family Predictive utility metrics
#'
#' @export
#'
pred_mae <- function(pred) {

  stopifnot(inherits(pred, "pred"))

  outcome <- .pred_outcome_name(pred)

  pred$predictions |>
    dplyr::group_by(.data$source) |>
    yardstick::mae(truth = !!rlang::sym(outcome), estimate = .pred) |>
    dplyr::ungroup()

}

#' Scatterplot comparing confidential and synthetic predictions for a `pred` object
#'
#' For regression models, plots confidential predictions (x-axis) against
#' synthetic predictions (y-axis). For classification models, plots
#' confidential predicted probabilities (x-axis) against synthetic predicted
#' probabilities (y-axis), colored by whether the predicted classes agree.
#' Only binary classification models are supported.
#'
#' @param pred A `pred` object created by `util_pred()`.
#' @param coord_equal Boolean for whether to fix the aspect ratio to 1:1 with
#' `ggplot2::coord_equal()`. Defaults to `TRUE`.
#'
#' @return A `ggplot2` plot.
#'
#' @family Predictive utility metrics
#'
#' @export
#'
pred_visualize <- function(pred, coord_equal = TRUE) {

  stopifnot(inherits(pred, "pred"))

  is_classification <- ".pred_class" %in% names(pred$predictions)

  # a row identifier is needed to align each holdout observation's
  # confidential and synthetic predictions, which are stored in long form
  predictions <- pred$predictions |>
    dplyr::group_by(.data$source) |>
    dplyr::mutate(.id = dplyr::row_number()) |>
    dplyr::ungroup()

  if (is_classification) {

    prob_cols <- setdiff(
      grep("^\\.pred_", names(predictions), value = TRUE),
      ".pred_class"
    )

    # a scatterplot of probabilities only makes sense for two classes
    if (length(prob_cols) != 2) {

      stop(
        "pred_visualize() only supports binary classification models.",
        call. = FALSE
      )

    }

    outcome <- .pred_outcome_name(pred)
    event_level <- levels(predictions[[outcome]])[1]
    prob_col <- paste0(".pred_", event_level)

    wide <- predictions |>
      dplyr::select(".id", "source", ".pred_class", !!rlang::sym(prob_col)) |>
      tidyr::pivot_wider(
        id_cols = ".id",
        names_from = "source",
        values_from = c(".pred_class", prob_col)
      ) |>
      dplyr::mutate(
        match = .data$.pred_class_confidential == .data$.pred_class_synthetic
      )

    plot <- ggplot2::ggplot(
      wide,
      ggplot2::aes(
        x = !!rlang::sym(paste0(prob_col, "_confidential")),
        y = !!rlang::sym(paste0(prob_col, "_synthetic")),
        color = .data$match
      )
    ) +
      ggplot2::geom_point(alpha = 0.5) +
      ggplot2::labs(
        x = "Confidential predicted probability",
        y = "Synthetic predicted probability",
        color = "Predicted classes match"
      )

  } else {

    wide <- predictions |>
      dplyr::select(".id", "source", ".pred") |>
      tidyr::pivot_wider(
        id_cols = ".id",
        names_from = "source",
        values_from = ".pred"
      )

    plot <- ggplot2::ggplot(
      wide,
      ggplot2::aes(x = .data$confidential, y = .data$synthetic)
    ) +
      ggplot2::geom_point(alpha = 0.5) +
      ggplot2::labs(
        x = "Confidential prediction",
        y = "Synthetic prediction"
      )

  }

  # probabilities and predictions share a scale with the identity line, so
  # a fixed aspect ratio makes the two axes visually comparable
  if (coord_equal) {

    plot <- plot + ggplot2::coord_equal()

  }

  plot

}
