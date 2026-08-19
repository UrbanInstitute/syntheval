
# shared toy fixtures ---------------------------------------

set.seed(20260818)

n <- 60

toy_conf <- data.frame(
  x1 = rnorm(n),
  x2 = rnorm(n),
  y_num = rnorm(n),
  y_cat = factor(sample(c("A", "B"), size = n, replace = TRUE), levels = c("A", "B"))
)

toy_synth <- data.frame(
  x1 = rnorm(n),
  x2 = rnorm(n),
  y_num = rnorm(n),
  y_cat = factor(sample(c("A", "B"), size = n, replace = TRUE), levels = c("A", "B"))
)

toy_holdout <- data.frame(
  x1 = rnorm(n),
  x2 = rnorm(n),
  y_num = rnorm(n),
  y_cat = factor(sample(c("A", "B"), size = n, replace = TRUE), levels = c("A", "B"))
)

toy_ed <- eval_data(conf_data = toy_conf, synth_data = toy_synth)

toy_ed_holdout <- eval_data(
  conf_data = toy_conf,
  synth_data = toy_synth,
  holdout_data = toy_holdout
)

reg_workflow <- workflows::workflow() |>
  workflows::add_formula(y_num ~ x1 + x2) |>
  workflows::add_model(parsnip::set_mode(parsnip::linear_reg(), "regression"))

class_workflow <- workflows::workflow() |>
  workflows::add_formula(y_cat ~ x1 + x2) |>
  workflows::add_model(parsnip::set_mode(parsnip::logistic_reg(), "classification"))

# attribute_target input errors ---------------------------------------

test_that("attribute_target input errors", {
  
  expect_error(
    attribute_target("not an eval_data object", workflow = reg_workflow)
  )
  
  expect_error(
    attribute_target(toy_ed, workflow = "not a workflow")
  )
  
})

# attribute_target object structure ---------------------------------------

test_that("attribute_target object structure without holdout data", {
  
  target <- attribute_target(toy_ed, workflow = reg_workflow)
  
  expect_s3_class(target, "attribute_target")
  expect_true(is_attribute_target(target))
  
  expect_identical(target$target_var, "y_num")
  expect_identical(target$mode, "regression")
  expect_null(target$holdout)
  expect_false(target$metadata$tuned)
  
  expect_identical(
    names(target$synthetic),
    c("fitted_model", "split", "predictions_train", "predictions_test", "predictions_confidential", "tuned")
  )
  
  expect_equal(
    nrow(target$synthetic$predictions_confidential),
    nrow(toy_conf)
  )
  
})

test_that("attribute_target object structure with holdout data", {
  
  target <- attribute_target(toy_ed_holdout, workflow = class_workflow)
  
  expect_identical(target$target_var, "y_cat")
  expect_identical(target$mode, "classification")
  expect_false(is.null(target$holdout))
  
  expect_equal(
    nrow(target$holdout$predictions_confidential),
    nrow(toy_conf)
  )
  
  # train + test rows reconstruct the full holdout fitting data
  expect_equal(
    nrow(target$holdout$predictions_train) + nrow(target$holdout$predictions_test),
    nrow(toy_holdout)
  )
  
})

test_that("attribute_target fit_holdout = FALSE skips the holdout branch", {
  
  target <- attribute_target(toy_ed_holdout, workflow = reg_workflow, fit_holdout = FALSE)
  
  expect_null(target$holdout)
  
})

test_that("attribute_target save_fit = FALSE drops fitted models", {
  
  target <- attribute_target(toy_ed_holdout, workflow = reg_workflow, save_fit = FALSE)
  
  expect_null(target$synthetic$fitted_model)
  expect_null(target$holdout$fitted_model)
  
})

test_that("attribute_target n_rep > 1 uses the first replicate with a message", {
  
  ed_multi_rep <- eval_data(
    conf_data = toy_conf,
    synth_data = list(toy_synth, toy_synth)
  )
  
  expect_message(
    target <- attribute_target(ed_multi_rep, workflow = reg_workflow)
  )
  
  expect_equal(target$metadata$n_rep, 2)
  
})

test_that("attribute_target tuning fits a finalized workflow and records tuned = TRUE", {
  
  tune_workflow <- workflows::workflow() |>
    workflows::add_formula(y_num ~ x1 + x2) |>
    workflows::add_model(
      parsnip::linear_reg(penalty = tune::tune(), mixture = 1) |>
        parsnip::set_engine("glmnet") |>
        parsnip::set_mode("regression")
    )
  
  grid <- dials::grid_regular(dials::penalty(), levels = 3)
  
  target <- attribute_target(toy_ed, workflow = tune_workflow, grid = grid, v = 3)
  
  expect_true(target$metadata$tuned)
  expect_true(target$synthetic$tuned)
  
})
