
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

class_target <- attribute_target(toy_ed, workflow = class_workflow)
class_target_holdout <- attribute_target(toy_ed_holdout, workflow = class_workflow)

# target_accuracy ---------------------------------------

test_that("target_accuracy requires a classification target", {
  
  expect_error(target_accuracy(reg_target))
  
})

test_that("target_accuracy returns one row per available source", {
  
  result <- target_accuracy(class_target)
  
  expect_identical(names(result), c("source", "on", "accuracy"))
  expect_identical(result$source, "synthetic")
  expect_true(result$accuracy >= 0 && result$accuracy <= 1)
  
  result_holdout <- target_accuracy(class_target_holdout)
  
  expect_identical(result_holdout$source, c("synthetic", "holdout"))
  
})

test_that("target_accuracy supports on = 'train'/'test'", {
  
  result <- target_accuracy(class_target, on = "train")
  
  expect_identical(result$on, "train")
  
})
