
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

reg_target <- attribute_target(toy_ed, workflow = reg_workflow)
reg_target_holdout <- attribute_target(toy_ed_holdout, workflow = reg_workflow)

class_target <- attribute_target(toy_ed, workflow = class_workflow)

# target_rmse ---------------------------------------

test_that("target_rmse requires a regression target", {
  
  expect_error(target_rmse(class_target))
  
})

test_that("target_rmse returns a non-negative tibble with one row per source", {
  
  result <- target_rmse(reg_target)
  
  expect_identical(names(result), c("source", "on", "rmse"))
  expect_identical(result$source, "synthetic")
  expect_true(result$rmse >= 0)
  
  result_holdout <- target_rmse(reg_target_holdout)
  
  expect_identical(result_holdout$source, c("synthetic", "holdout"))
  
})
