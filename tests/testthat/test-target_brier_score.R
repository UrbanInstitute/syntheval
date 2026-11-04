
# fixtures ---------------------------------------

# x1 perfectly separates the classes, so rpart grows pure terminal nodes and
# predicts probabilities of exactly 0 and 1; `flip` inverts that share of each
# class while holding x1 fixed, so a model fit on unflipped data scores a Brier
# score of exactly `flip` against it
separable_data <- function(n, flip = 0) {
  
  y_cat <- factor(rep(c("A", "B"), each = n / 2), levels = c("A", "B"))
  x1 <- ifelse(y_cat == "A", 1, -1)
  
  n_flip <- n / 2 * flip
  flip_index <- c(seq_len(n_flip), n / 2 + seq_len(n_flip))
  
  y_cat[flip_index] <- ifelse(y_cat[flip_index] == "A", "B", "A")
  
  data.frame(x1 = x1, y_num = seq_len(n), y_cat = y_cat)
  
}

reg_workflow <- workflows::workflow() |>
  workflows::add_formula(y_num ~ x1) |>
  workflows::add_model(parsnip::set_mode(parsnip::linear_reg(), "regression"))

tree_workflow <- workflows::workflow() |>
  workflows::add_formula(y_cat ~ x1) |>
  workflows::add_model(parsnip::set_mode(parsnip::decision_tree(), "classification"))

# fit on unflipped synthetic data, attack confidential data with `flip` inverted
separable_target <- function(flip = 0, n = 400) {
  
  attribute_target(
    eval_data(
      conf_data = separable_data(n, flip = flip),
      synth_data = separable_data(n)
    ),
    workflow = tree_workflow
  )
  
}

brier_at <- function(result, prediction_data, modeling_data = "synthetic") {
  
  result$brier_score[
    result$prediction_data %in% prediction_data &
      result$modeling_data %in% modeling_data
  ]
  
}

# target_brier_score ---------------------------------------

test_that("target_brier_score requires a classification target", {
  
  reg_target <- attribute_target(
    eval_data(conf_data = separable_data(60), synth_data = separable_data(60)),
    workflow = reg_workflow
  )
  
  expect_error(target_brier_score(reg_target))
  
})

test_that("target_brier_score returns a non-negative tibble", {
  
  skip_if_not_installed("rpart")
  
  result <- target_brier_score(separable_target(flip = 0.5))
  
  expect_identical(names(result), c("modeling_data", "prediction_data", "n", "brier_score"))
  expect_true(all(result$brier_score >= 0))
  
})

# scale convention ---------------------------------------

test_that("target_brier_score matches mean squared error on the event level", {
  
  skip_if_not_installed("rpart")
  
  target <- separable_target(flip = 0.5)
  predictions <- target$synthetic$predictions_confidential
  
  manual <- mean((as.numeric(predictions$y_cat == "A") - predictions$.pred_A)^2)
  
  expect_equal(brier_at(target_brier_score(target), "confidential"), manual)
  
})

# systematic bounds ---------------------------------------

test_that("target_brier_score equals the share of flipped labels", {
  
  skip_if_not_installed("rpart")
  
  # 0 and 1 are the floor and ceiling only under the mean-squared-error
  # convention; summing squared error across classes would double them
  expect_equal(brier_at(target_brier_score(separable_target(0)), "confidential"), 0)
  expect_equal(brier_at(target_brier_score(separable_target(0.5)), "confidential"), 0.5)
  expect_equal(brier_at(target_brier_score(separable_target(1)), "confidential"), 1)
  
})

# identical holdout ---------------------------------------

test_that("an identical holdout does not perturb the synthetic branch", {
  
  skip_if_not_installed("rpart")
  
  conf <- separable_data(400, flip = 0.5)
  synth <- separable_data(400)
  
  ed_without <- eval_data(conf_data = conf, synth_data = synth)
  ed_with <- eval_data(conf_data = conf, synth_data = synth, holdout_data = synth)
  
  # the synthetic branch is fit first in both calls, so a shared seed makes it
  # identical unless the holdout branch leaks into it
  set.seed(1)
  without <- target_brier_score(attribute_target(ed_without, workflow = tree_workflow))
  
  set.seed(1)
  with <- target_brier_score(attribute_target(ed_with, workflow = tree_workflow))
  
  expect_equal(without$brier_score, brier_at(with, c("training", "testing", "confidential")))
  
})
