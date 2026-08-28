test_wf <- workflows::workflow() |>
  workflows::add_recipe(
    recipes::recipe(hcovany ~ age + sex + empstat, data = acs_conf) |>
      recipes::step_dummy(recipes::all_factor_predictors())
  ) |>
  workflows::add_model(
    parsnip::logistic_reg() |> 
      parsnip::set_engine("glm")
  )

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
  expect_setequal(unique(pred$predictions$source), c("confidential", "synthetic"))
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
    dplyr::filter(source == "confidential") |>
    dplyr::pull(.pred_class)

  synth_preds <- pred$predictions |>
    dplyr::filter(source == "synthetic") |>
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
      dplyr::filter(source == "confidential") |>
      nrow(),
    nrow(acs_holdout)
  )

  expect_equal(
    pred$predictions |>
      dplyr::filter(source == "synthetic") |>
      nrow(),
    nrow(acs_holdout)
  )

  # truth column should carry over the observed holdout outcome
  expect_equal(
    pred$predictions |>
      dplyr::filter(source == "confidential") |>
      dplyr::pull(hcovany),
    acs_holdout$hcovany
  )

  # the two models are fit on different data, so predictions need not match
  conf_preds <- pred$predictions |>
    dplyr::filter(source == "confidential") |>
    dplyr::pull(.pred_class)

  synth_preds <- pred$predictions |>
    dplyr::filter(source == "synthetic") |>
    dplyr::pull(.pred_class)

  expect_false(isTRUE(all.equal(conf_preds, synth_preds)))

})

test_that("util_pred errors without holdout_data (no-holdout procedure not yet implemented)", {

  ed <- eval_data(
    conf_data = acs_conf,
    synth_data = acs_conf
  )

  expect_error(util_pred(ed, workflow = test_wf))

})
