
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
class_target_holdout <- attribute_target(toy_ed_holdout, workflow = class_workflow)

# .get_target_predictions ---------------------------------------

test_that(".get_target_predictions errors on a missing branch", {
  
  expect_error(
    .get_target_predictions(reg_target, source = "holdout", on = "confidential")
  )
  
})

test_that(".get_target_predictions retrieves the requested branch/on combination", {
  
  train_predictions <- .get_target_predictions(reg_target, source = "synthetic", on = "train")
  test_predictions <- .get_target_predictions(reg_target, source = "synthetic", on = "test")
  conf_predictions <- .get_target_predictions(reg_target, source = "synthetic", on = "confidential")
  
  expect_equal(nrow(train_predictions) + nrow(test_predictions), nrow(toy_synth))
  expect_equal(nrow(conf_predictions), nrow(toy_conf))
  
})

# .available_sources ---------------------------------------

test_that(".available_sources reflects whether a holdout branch exists", {
  
  expect_identical(.available_sources(reg_target), "synthetic")
  expect_identical(.available_sources(reg_target_holdout), c("synthetic", "holdout"))
  
})

# .class_prob_estimate ---------------------------------------

test_that(".class_prob_estimate returns a vector for binary and a matrix for multiclass", {
  
  predictions <- .get_target_predictions(class_target, source = "synthetic", on = "confidential")
  
  binary_estimate <- .class_prob_estimate(predictions, "y_cat")
  
  expect_true(is.numeric(binary_estimate))
  expect_null(dim(binary_estimate))
  
  three_level_predictions <- predictions
  levels(three_level_predictions$y_cat) <- c("A", "B", "C")
  three_level_predictions$.pred_C <- 0
  
  multiclass_estimate <- .class_prob_estimate(three_level_predictions, "y_cat")
  
  expect_true(is.matrix(multiclass_estimate))
  expect_equal(ncol(multiclass_estimate), 3)
  
})
