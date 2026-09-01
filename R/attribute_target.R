#' 
#' Extract the outcome/target variable name from a workflow's preprocessor
#' 
#' @param workflow A `workflows::workflow`.
#' 
#' @return A character scalar with the outcome variable name.
#' 
#' @noRd
#' 
.workflow_target_var <- function(workflow) {
  
  preprocessor <- workflows::extract_preprocessor(workflow)
  
  if (inherits(preprocessor, "recipe")) {
    
    target_var <- preprocessor$var_info$variable[preprocessor$var_info$role == "outcome"]
    
  } else if (inherits(preprocessor, "formula")) {
    
    target_var <- all.vars(preprocessor)[1]
    
  } else {
    
    stop("Error: workflow must be built with workflows::add_recipe() or workflows::add_formula().")
    
  }
  
  stopifnot(length(target_var) == 1)
  
  return(target_var)
  
}

#' 
#' Extract and validate the model mode from a workflow's model spec
#' 
#' @param workflow A `workflows::workflow`.
#' 
#' @return A character scalar, either `"classification"` or `"regression"`.
#' 
#' @noRd
#' 
.workflow_mode <- function(workflow) {
  
  spec <- workflows::extract_spec_parsnip(workflow)
  
  if (!spec$mode %in% c("classification", "regression")) {
    
    stop("Error: workflow's model spec must have mode 'classification' or 'regression'. Use parsnip::set_mode().")
    
  }
  
  return(spec$mode)
  
}

#' 
#' Fit a workflow on one data source and generate train/test/confidential predictions
#' 
#' @param data A data frame to split, fit the workflow on, and generate 
#' train/test diagnostic predictions from (e.g. synthetic or holdout data).
#' @param workflow A `workflows::workflow`.
#' @param target_var The outcome variable name.
#' @param mode Either `"classification"` or `"regression"`.
#' @param conf_data The confidential data frame to generate attack predictions on.
#' @param prop The proportion of `data` retained for training in the internal 
#' train/test split.
#' @param grid An optional tibble of hyperparameters for tuning.
#' @param v The number of folds for cross-validation when `grid` is supplied.
#' 
#' @return A list with a fitted model, the `rsample` split, and three 
#' predictions tibbles (train, test, confidential).
#' 
#' @noRd
#' 
.fit_target_branch <- function(data, workflow, target_var, mode, conf_data, prop, grid, v) {
  
  strata <- if (mode == "classification") target_var else NULL
  
  data_split <- rsample::initial_split(data = data, prop = prop, strata = strata)
  
  training_data <- rsample::training(data_split)
  testing_data <- rsample::testing(data_split)
  
  if (is.null(grid)) {
    
    fitted_model <- parsnip::fit(workflow, data = training_data)
    tuned <- FALSE
    
  } else {
    
    folds <- rsample::vfold_cv(data = training_data, v = v)
    
    tune_metric <- if (mode == "classification") "roc_auc" else "rmse"
    
    tune_results <- tune::tune_grid(workflow, resamples = folds, grid = grid)
    
    finalized_workflow <- tune::finalize_workflow(
      workflow, 
      tune::select_best(tune_results, metric = tune_metric)
    )
    
    fitted_model <- parsnip::fit(finalized_workflow, data = training_data)
    tuned <- TRUE
    
  }
  
  predict_branch <- function(new_data) {
    
    if (mode == "classification") {
      
      dplyr::bind_cols(
        dplyr::select(new_data, dplyr::all_of(target_var)),
        stats::predict(fitted_model, new_data = new_data, type = "class"),
        stats::predict(fitted_model, new_data = new_data, type = "prob")
      )
      
    } else {
      
      dplyr::bind_cols(
        dplyr::select(new_data, dplyr::all_of(target_var)),
        stats::predict(fitted_model, new_data = new_data, type = "numeric")
      )
      
    }
    
  }
  
  result <- list(
    fitted_model = fitted_model,
    split = data_split,
    predictions_train = predict_branch(training_data),
    predictions_test = predict_branch(testing_data),
    predictions_confidential = predict_branch(conf_data),
    tuned = tuned
  )
  
  return(result)
  
}

#' 
#' Create an attribute_target object for a model-based known-target attack
#' 
#' @param eval_data An `eval_data` object.
#' @param workflow A `workflows::workflow` with a model added via 
#' `workflows::add_model()` and a preprocessor added via 
#' `workflows::add_recipe()` or `workflows::add_formula()`. The model spec's 
#' mode must be `"classification"` or `"regression"`. Build the recipe/formula 
#' against `common_data(eval_data)` rather than the confidential, synthetic, or 
#' holdout data directly, so the workflow only uses variables (and factor 
#' levels) present across all of them.
#' @param prop The proportion of each fitting data set (synthetic and, if 
#' available, holdout) retained for training in an internal train/test split. 
#' Defaults to 3/4.
#' @param grid An optional tibble of hyperparameters for tuning. When supplied, 
#' the workflow is tuned via cross-validation on the training split before the 
#' final fit.
#' @param v The number of cross-validation folds used when `grid` is supplied. 
#' Defaults to 10.
#' @param fit_holdout A logical for if the workflow should also be fit on 
#' `eval_data$holdout_data` when it is supplied. Defaults to `TRUE`.
#' @param save_fit A logical for if the fitted model objects should be kept in 
#' the returned object. Defaults to `TRUE`.
#' 
#' @returns An `attribute_target` object. If `eval_data$holdout_data` is 
#' supplied and `fit_holdout` is `TRUE`, the object also includes a `holdout` 
#' element with the same structure as `synthetic`.
#' 
#' @export
#' 
attribute_target <- function(
    eval_data,
    workflow,
    prop = 3 / 4,
    grid = NULL,
    v = 10,
    fit_holdout = TRUE,
    save_fit = TRUE
) {
  
  stopifnot(is_eval_data(eval_data))
  stopifnot(inherits(workflow, "workflow"))
  
  if (eval_data$n_rep > 1) {
    
    synth_data <- eval_data$synth_data[[1]]
    message("Creating attribute_target object using 1 synthetic data replicate.")
    
  } else {
    
    synth_data <- eval_data$synth_data
    
  }
  
  conf_data <- eval_data$conf_data
  
  target_var <- .workflow_target_var(workflow)
  mode <- .workflow_mode(workflow)
  
  stopifnot(target_var %in% names(synth_data))
  stopifnot(target_var %in% names(conf_data))
  
  result <- list(
    workflow = workflow,
    mode = mode,
    target_var = target_var,
    synthetic = .fit_target_branch(
      data = synth_data,
      workflow = workflow,
      target_var = target_var,
      mode = mode,
      conf_data = conf_data,
      prop = prop,
      grid = grid,
      v = v
    )
  )
  
  # holdout data is optional; only fit a second branch when available and requested
  if (!is.null(eval_data$holdout_data) && fit_holdout) {
    
    stopifnot(target_var %in% names(eval_data$holdout_data))
    
    result$holdout <- .fit_target_branch(
      data = eval_data$holdout_data,
      workflow = workflow,
      target_var = target_var,
      mode = mode,
      conf_data = conf_data,
      prop = prop,
      grid = grid,
      v = v
    )
    
  } else {
    
    result$holdout <- NULL
    
  }
  
  if (!save_fit) {
    
    result$synthetic$fitted_model <- NULL
    
    if (!is.null(result$holdout)) {
      
      result$holdout$fitted_model <- NULL
      
    }
    
  }
  
  result$metadata <- list(
    prop = prop,
    v = v,
    tuned = !is.null(grid),
    n_rep = eval_data$n_rep
  )
  
  result$call <- match.call()
  
  result <- structure(result, class = "attribute_target")
  
  return(result)
  
}

#' 
#' Check if object is `attribute_target`
#' 
#' @param x object
#' 
#' @return A boolean
#' 
#' @export
#' 
is_attribute_target <- function(x) {
  inherits(x, "attribute_target")
}

#' @export
print.attribute_target <- function(x, ...) {
  
  cat("Attribute Target \n")
  cat("Target Variable: ", x$target_var, "\n")
  cat("Mode: ", x$mode, "\n")
  cat("Tuned: ", x$metadata$tuned, "\n")
  
  cat(
    "Synthetic Confidential Predictions: ", 
    nrow(x$synthetic$predictions_confidential), 
    "\n"
  )
  
  if (!is.null(x$holdout)) {
    
    cat(
      "Holdout Confidential Predictions: ", 
      nrow(x$holdout$predictions_confidential), 
      "\n"
    )
    
  }
  
  invisible(x)
  
}
