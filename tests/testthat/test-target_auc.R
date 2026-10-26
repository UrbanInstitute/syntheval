
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

toy_ed <- eval_data(conf_data = toy_conf, synth_data = toy_synth)

reg_workflow <- workflows::workflow() |>
  workflows::add_formula(y_num ~ x1 + x2) |>
  workflows::add_model(parsnip::set_mode(parsnip::linear_reg(), "regression"))

class_workflow <- workflows::workflow() |>
  workflows::add_formula(y_cat ~ x1 + x2) |>
  workflows::add_model(parsnip::set_mode(parsnip::logistic_reg(), "classification"))

reg_target <- attribute_target(toy_ed, workflow = reg_workflow)
class_target <- attribute_target(toy_ed, workflow = class_workflow)

# target_auc ---------------------------------------

test_that("target_auc requires a classification target", {
  
  expect_error(target_auc(reg_target))
  
})

test_that("target_auc returns one row per modeling/prediction data combination", {
  
  result <- target_auc(class_target)
  
  expect_identical(names(result), c("modeling_data", "prediction_data", "n", "auc"))
  expect_identical(
    result$modeling_data, 
    factor(rep("synthetic", 3), levels = "synthetic")
  )
  expect_identical(
    result$prediction_data,
    factor(
      c("training", "testing", "confidential"),
      levels = c("training", "testing", "confidential")
    )
  )
  expect_true(all(result$auc >= 0 & result$auc <= 1))
  
  # training + testing reconstruct the modeling data; confidential is all records
  expect_equal(sum(result$n[1:2]), nrow(toy_synth))
  expect_equal(result$n[3], nrow(toy_conf))
  
})

test_that("target_auc includes holdout rows when a holdout branch exists", {
  
  toy_ed_holdout <- eval_data(
    conf_data = toy_conf,
    synth_data = toy_synth,
    holdout_data = toy_conf
  )
  
  result <- target_auc(attribute_target(toy_ed_holdout, workflow = class_workflow))
  
  expect_identical(
    result$modeling_data, 
    factor(rep(c("synthetic", "holdout"), each = 3), levels = c("synthetic", "holdout"))
  )
  
})
