
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

# target_rsq ---------------------------------------

test_that("target_rsq requires a regression target", {
  
  expect_error(target_rsq(class_target))
  
})

test_that("target_rsq returns a tibble", {
  
  result <- target_rsq(reg_target)
  
  expect_identical(names(result), c("source", "on", "rsq"))
  expect_true(is.numeric(result$rsq))
  
})
