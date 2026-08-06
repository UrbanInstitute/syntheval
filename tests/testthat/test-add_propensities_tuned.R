test_that("add_propensities_tuned returns propensities and fitted workflow", {

  skip_if_not_installed("glmnet")

  logistic_mod <- parsnip::logistic_reg(penalty = tune::tune()) |>
    parsnip::set_mode(mode = "classification") |>
    parsnip::set_engine(engine = "glmnet")

  # build evaluation/discrimination object using small example data
  ed <- eval_data(conf_data = penguins_conf, synth_data = penguins_postsynth)
  disc <- discrimination(ed)

  rec <- recipes::recipe(.source_label ~ ., data = disc$combined_data) |>
    recipes::step_dummy(recipes::all_nominal_predictors())

  # very small tuning grid and 2-fold CV for speed in tests
  grid <- tibble::tibble(penalty = c(1e-3, 1e-2))

  set.seed(123)
  out <- disc |>
    add_propensities_tuned(
      spec = logistic_mod,
      recipe = rec,
      grid = grid,
      v = 2,
      prop = 0.7,
      save_fit = TRUE
    )

  # basic structural assertions
  expect_s3_class(out, "discrimination")
  expect_true("propensities" %in% names(out))

  p <- out[["propensities"]]
  expect_equal(nrow(p), nrow(disc[["combined_data"]]))
  expect_true(".pred_synthetic" %in% colnames(p))
  expect_type(p$.pred_synthetic, "double")
  expect_true(all(p$.pred_synthetic >= 0 & p$.pred_synthetic <= 1))
  expect_true(".sample" %in% colnames(p))
  expect_true(all(p$.sample %in% c("training", "testing")))

  # fitted workflow should be stored on the discrimination object
  expect_s3_class(out$discriminator, "workflow")

})
