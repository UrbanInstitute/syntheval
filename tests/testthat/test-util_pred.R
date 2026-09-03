test_wf <- workflows::workflow() |>
  workflows::add_recipe(
    recipes::recipe(sex ~ age + hcovany + empstat, data = acs_conf) |>
      recipes::step_dummy(recipes::all_factor_predictors())
  ) |>
  workflows::add_model(
    parsnip::logistic_reg() |> 
      parsnip::set_engine("glm")
  )

regression_wf <- workflows::workflow() |>
  workflows::add_recipe(
    recipes::recipe(inctot ~ age + empstat, data = acs_conf) |>
      recipes::step_dummy(recipes::all_factor_predictors())
  ) |>
  workflows::add_model(
    parsnip::linear_reg() |>
      parsnip::set_engine("lm")
  )

test_that("util_pred uses the split procedure when holdout_data is not supplied", {
  
  ed <- eval_data(
    conf_data = acs_conf,
    synth_data = acs_conf
  )
  
  pred <- util_pred(ed, workflow = test_wf)
  
  expect_s3_class(pred, "pred")
  expect_equal(pred$procedure, "split")
  
  # a model fit on both the confidential and synthetic data
  expect_named(pred$models, c("confidential", "synthetic"), ignore.order = TRUE)
  
  # both fitted models generate predictions on both implementation partitions
  expect_setequal(
    unique(pred$predictions$model), 
    c("confidential", "synthetic")
  )
  expect_setequal(
    unique(pred$predictions$data), 
    c("confidential", "synthetic")
  )
  
  # confidential and synthetic models are both scored on the confidential
  # implementation data, and both scored on the synthetic implementation data
  expect_equal(
    sum(pred$predictions$model == "confidential" & pred$predictions$data == "confidential"),
    sum(pred$predictions$model == "synthetic" & pred$predictions$data == "confidential")
  )
  expect_equal(
    sum(pred$predictions$model == "confidential" & pred$predictions$data == "synthetic"),
    sum(pred$predictions$model == "synthetic" & pred$predictions$data == "synthetic")
  )
  
})

test_that("util_pred split procedure scores all four model x data combinations", {
  
  ed <- eval_data(
    conf_data = acs_conf,
    synth_data = acs_lr_synths[[1]]
  )
  
  pred <- util_pred(ed, workflow = test_wf)
  
  covariates_by <- function(model_name, data_name) {
    pred$predictions |>
      dplyr::filter(model == model_name, data == data_name) |>
      dplyr::select(age, hcovany, empstat)
  }
  
  # confidential and synthetic models are both predictions on the
  # confidential data, just from different fitted models
  expect_equal(covariates_by("confidential", "confidential"), covariates_by("synthetic", "confidential"))
  
  # confidential and synthetic models are both predictions on the
  # synthetic data, just from different fitted models
  expect_equal(
    covariates_by("confidential", "synthetic"), 
    covariates_by("synthetic", "synthetic")
  )
  
})

test_that("util_pred split procedure computes probabilities from the matching fitted model", {
  
  ed <- eval_data(
    conf_data = acs_conf,
    synth_data = acs_lr_synths[[1]]
  )
  
  pred <- util_pred(ed, workflow = test_wf)
  
  manual_probs <- function(model_name, data_name) {
    rows <- pred$predictions |> dplyr::filter(model == model_name, data == data_name)
    stats::predict(pred$models[[model_name]], new_data = rows, type = "prob")$.pred_Female
  }
  
  # every model x data combination's probabilities must come from its own
  # model, not the other model
  expect_equal(
    pred$predictions |> dplyr::filter(model == "confidential", data == "confidential") |> dplyr::pull(.pred_Female),
    manual_probs("confidential", "confidential")
  )
  expect_equal(
    pred$predictions |> dplyr::filter(model == "synthetic", data == "confidential") |> dplyr::pull(.pred_Female),
    manual_probs("synthetic", "confidential")
  )
  expect_equal(
    pred$predictions |> dplyr::filter(model == "confidential", data == "synthetic") |> dplyr::pull(.pred_Female),
    manual_probs("confidential", "synthetic")
  )
  expect_equal(
    pred$predictions |> dplyr::filter(model == "synthetic", data == "synthetic") |> dplyr::pull(.pred_Female),
    manual_probs("synthetic", "synthetic")
  )
  
})

test_that("util_pred equalize_data equalizes the confidential and synthetic row counts before splitting", {
  
  small_synth <- dplyr::slice_head(acs_conf, n = nrow(acs_conf) / 2)
  
  ed <- eval_data(
    conf_data = acs_conf,
    synth_data = small_synth
  )
  
  pred <- util_pred(ed, workflow = test_wf, equalize_data = TRUE)
  
  # the confidential and synthetic implementation partitions come from
  # equally-sized data once equalize_data resamples both down to the
  # smaller row count
  expect_equal(
    sum(pred$predictions$data == "confidential"),
    sum(pred$predictions$data == "synthetic")
  )
  
})

test_that("util_pred errors when equalize_data is TRUE and holdout_data is supplied", {
  
  ed <- eval_data(
    conf_data = acs_conf,
    synth_data = acs_conf,
    holdout_data = acs_conf
  )
  
  expect_error(
    util_pred(ed, workflow = test_wf, equalize_data = TRUE),
    "equalize_data not supported"
  )
  
})

test_that("util_pred uses the holdout procedure when holdout_data is supplied", {

  ed <- eval_data(
    conf_data = acs_conf,
    synth_data = acs_conf,
    holdout_data = acs_conf
  )

  pred <- util_pred(ed, workflow = test_wf)

  expect_s3_class(pred, "pred")
  expect_equal(pred$procedure, "holdout")

  # a model fit on both the confidential and synthetic data
  expect_named(pred$models, c("confidential", "synthetic"), ignore.order = TRUE)

  # predictions made on the holdout data for both models, and nothing else
  expect_setequal(unique(pred$predictions$model), c("confidential", "synthetic"))
  expect_true(all(pred$predictions$data == "holdout"))
  expect_equal(nrow(pred$predictions), nrow(acs_conf) * 2)

})

test_that("util_pred holdout predictions match when confidential, synthetic, and holdout data are identical", {

  ed <- eval_data(
    conf_data = acs_conf,
    synth_data = acs_conf,
    holdout_data = acs_conf
  )

  pred <- util_pred(ed, workflow = test_wf)

  conf_preds <- pred$predictions |>
    dplyr::filter(model == "confidential") |>
    dplyr::pull(.pred_class)

  synth_preds <- pred$predictions |>
    dplyr::filter(model == "synthetic") |>
    dplyr::pull(.pred_class)

  # identical training and holdout data should produce identical predictions
  expect_equal(conf_preds, synth_preds)

})

test_that("util_pred holdout predictions differ when synthetic data differ from confidential data", {

  ed <- eval_data(
    conf_data = acs_conf,
    synth_data = acs_lr_synths[[1]],
    holdout_data = acs_holdout
  )

  pred <- util_pred(ed, workflow = test_wf)

  expect_s3_class(pred, "pred")
  expect_equal(pred$procedure, "holdout")

  # predictions are generated for every holdout row from both models
  expect_equal(
    pred$predictions |>
      dplyr::filter(model == "confidential") |>
      nrow(),
    nrow(acs_holdout)
  )

  expect_equal(
    pred$predictions |>
      dplyr::filter(model == "synthetic") |>
      nrow(),
    nrow(acs_holdout)
  )

  # truth column should carry over the observed holdout outcome
  expect_equal(
    pred$predictions |>
      dplyr::filter(model == "confidential") |>
      dplyr::pull(sex),
    acs_holdout$sex
  )

  # the two models are fit on different data, so predictions need not match
  conf_preds <- pred$predictions |>
    dplyr::filter(model == "confidential") |>
    dplyr::pull(.pred_class)

  synth_preds <- pred$predictions |>
    dplyr::filter(model == "synthetic") |>
    dplyr::pull(.pred_class)

  expect_false(isTRUE(all.equal(conf_preds, synth_preds)))

})

test_that("pred_auc, pred_precision, and pred_recall return one row per model x data combination without holdout data", {
  
  ed <- eval_data(
    conf_data = acs_conf,
    synth_data = acs_lr_synths[[1]]
  )
  
  # sex is roughly balanced, so predictions naturally vary between classes
  pred <- util_pred(ed, workflow = test_wf)
  
  auc <- pred_auc(pred)
  precision <- pred_precision(pred)
  recall <- pred_recall(pred)
  
  expect_equal(nrow(auc), 4)
  expect_equal(nrow(precision), 4)
  expect_equal(nrow(recall), 4)
  
  expect_true(all(auc$.estimate >= 0 & auc$.estimate <= 1))
  expect_true(all(precision$.estimate >= 0 & precision$.estimate <= 1))
  expect_true(all(recall$.estimate >= 0 & recall$.estimate <= 1))
  
})

test_that("pred_auc, pred_precision, and pred_recall return one row per model with holdout data", {

  ed <- eval_data(
    conf_data = acs_conf,
    synth_data = acs_lr_synths[[1]],
    holdout_data = acs_holdout
  )

  # sex is roughly balanced, so predictions naturally vary between classes
  pred <- util_pred(ed, workflow = test_wf)

  auc <- pred_auc(pred)
  precision <- pred_precision(pred)
  recall <- pred_recall(pred)

  expect_setequal(auc$model, c("confidential", "synthetic"))
  expect_setequal(precision$model, c("confidential", "synthetic"))
  expect_setequal(recall$model, c("confidential", "synthetic"))

  expect_true(all(auc$.estimate >= 0 & auc$.estimate <= 1))
  expect_true(all(precision$.estimate >= 0 & precision$.estimate <= 1))
  expect_true(all(recall$.estimate >= 0 & recall$.estimate <= 1))

})

test_that("pred_auc, pred_precision, and pred_recall match when confidential and synthetic data are identical", {

  ed <- eval_data(
    conf_data = acs_conf,
    synth_data = acs_conf,
    holdout_data = acs_conf
  )

  pred <- util_pred(ed, workflow = test_wf)

  auc <- pred_auc(pred)
  precision <- pred_precision(pred)
  recall <- pred_recall(pred)

  expect_equal(
    auc$.estimate[auc$model == "confidential"],
    auc$.estimate[auc$model == "synthetic"]
  )

  expect_equal(
    precision$.estimate[precision$model == "confidential"],
    precision$.estimate[precision$model == "synthetic"]
  )

  expect_equal(
    recall$.estimate[recall$model == "confidential"],
    recall$.estimate[recall$model == "synthetic"]
  )

})

test_that("util_pred supports regression workflows and pred_rmse/pred_mae return one row per model", {

  ed <- eval_data(
    conf_data = acs_conf,
    synth_data = acs_lr_synths[[1]],
    holdout_data = acs_holdout
  )

  pred <- util_pred(ed, workflow = regression_wf)

  expect_s3_class(pred, "pred")
  expect_equal(pred$procedure, "holdout")

  # regression predictions have no class probabilities
  expect_false(any(c(".pred_class") %in% names(pred$predictions)))

  rmse <- pred_rmse(pred)
  mae <- pred_mae(pred)

  expect_setequal(rmse$model, c("confidential", "synthetic"))
  expect_setequal(mae$model, c("confidential", "synthetic"))

  expect_true(all(rmse$.estimate >= 0))
  expect_true(all(mae$.estimate >= 0))

})

test_that("pred_rmse and pred_mae match when confidential, synthetic, and holdout data are identical", {

  ed <- eval_data(
    conf_data = acs_conf,
    synth_data = acs_conf,
    holdout_data = acs_conf
  )

  pred <- util_pred(ed, workflow = regression_wf)

  rmse <- pred_rmse(pred)
  mae <- pred_mae(pred)

  expect_equal(
    rmse$.estimate[rmse$model == "confidential"],
    rmse$.estimate[rmse$model == "synthetic"]
  )

  expect_equal(
    mae$.estimate[mae$model == "confidential"],
    mae$.estimate[mae$model == "synthetic"]
  )

})

test_that("pred_visualize returns a scatterplot of confidential vs. synthetic probabilities for binary classification", {

  ed <- eval_data(
    conf_data = acs_conf,
    synth_data = acs_lr_synths[[1]],
    holdout_data = acs_holdout
  )

  pred <- util_pred(ed, workflow = test_wf)

  plot <- pred_visualize(pred)

  expect_s3_class(plot, "ggplot")
  expect_equal(nrow(plot$data), nrow(acs_holdout))
  expect_true("match" %in% names(plot$data))

})

test_that("pred_visualize returns a scatterplot of confidential vs. synthetic predictions for regression", {

  ed <- eval_data(
    conf_data = acs_conf,
    synth_data = acs_lr_synths[[1]],
    holdout_data = acs_holdout
  )

  pred <- util_pred(ed, workflow = regression_wf)

  plot <- pred_visualize(pred)

  expect_s3_class(plot, "ggplot")
  expect_equal(nrow(plot$data), nrow(acs_holdout))
  expect_named(plot$data, c(".id", "data", "confidential", "synthetic"))

})

test_that("pred_visualize errors for multi-class classification models", {

  multiclass_pred <- structure(
    list(
      procedure = "holdout",
      models = list(confidential = NULL, synthetic = NULL),
      predictions = dplyr::tibble(
        model = rep(c("confidential", "synthetic"), each = 3),
        data = "holdout",
        outcome = factor(rep(c("a", "b", "c"), 2)),
        .pred_class = factor(rep(c("a", "b", "c"), 2)),
        .pred_a = c(0.5, 0.2, 0.3, 0.5, 0.2, 0.3),
        .pred_b = c(0.3, 0.5, 0.2, 0.3, 0.5, 0.2),
        .pred_c = c(0.2, 0.3, 0.5, 0.2, 0.3, 0.5)
      )
    ),
    class = "pred"
  )

  # the prob-column count is checked before the outcome variable is looked
  # up, so this hand-built pred object doesn't need a fitted workflow
  expect_error(
    pred_visualize(multiclass_pred),
    "binary classification"
  )

})

