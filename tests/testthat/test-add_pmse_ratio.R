test_that("add_pmse returns ideal value for identical data with variation " , {
  
  set.seed(1)
  
  data <-
    data.frame(
      x = rnorm(n = 1000, mean = 0, sd = 1),
      y = rnorm(n = 1000, mean = 0, sd = 1)
    )

  postsynth <-
    list(
      synthetic_data = data,
      jth_synthesis_time = data.frame(
        variable = factor(c("x", "y"))
      )
    ) |>
    structure(class = "postsynth")
  
  ed <- eval_data(conf_data = data, synth_data = postsynth)
  
  dt_mod <- parsnip::decision_tree() |>
    parsnip::set_mode(mode = "classification") |>
    parsnip::set_engine(engine = "rpart")
  
  rec <- recipes::recipe(.source_label ~ ., data = discrimination(ed)$combined_data)
  
  disc <- suppressWarnings(
    discrimination(ed) |>
      add_propensities(
        recipe = rec,
        spec = dt_mod
      ) 
  )
  
  expect_error(add_pmse_ratio(disc))
  
  disc <- disc |>
    add_pmse(split = FALSE) |>
    add_pmse_ratio(split = FALSE, times = 25)
  
  expect_equal(round(disc$pmse$.pmse, digit = 1), 0)
  # this is a bad test but will at least tell us when the code logic changes
  expect_equal(round(disc$pmse$.pmse_ratio, 5), 0.56262)
  
  disc <- disc |>
    add_pmse() |>
    add_pmse_ratio(times = 25)
  
  expect_equal(round(disc$pmse$.pmse, digit = 2), c(0, 0))
  # this is a bad test but will at least tell us when the code logic changes
  expect_equal(round(disc$pmse$.pmse_ratio, 5), c(0.27909, 0.60349))
  
})

test_that("add_pmse_ratio method = 'logistic' matches closed-form Snoke et al. (2018) formula", {

  set.seed(1)

  data <-
    data.frame(
      x = rnorm(n = 1000, mean = 0, sd = 1),
      y = rnorm(n = 1000, mean = 0, sd = 1)
    )

  postsynth <-
    list(
      synthetic_data = data,
      jth_synthesis_time = data.frame(
        variable = factor(c("x", "y"))
      )
    ) |>
    structure(class = "postsynth")

  ed <- eval_data(conf_data = data, synth_data = postsynth)

  glm_mod <- parsnip::logistic_reg() |>
    parsnip::set_mode(mode = "classification") |>
    parsnip::set_engine(engine = "glm")

  rec <- recipes::recipe(.source_label ~ ., data = discrimination(ed)$combined_data)

  disc <- discrimination(ed) |>
      add_propensities(
        recipe = rec,
        spec = glm_mod
      ) |>
      add_pmse(split = FALSE) |>
      add_pmse_ratio(split = FALSE, method = "logistic")


  # closed-form null pMSE: (k - 1)(1 - c)^2 / N, k = 3 coefficients (intercept, x, y)
  propensities <- disc$propensities
  n <- nrow(propensities)
  c_prop <- mean(propensities$.source_label == "synthetic")
  expected_null_pmse <- (3 - 1) * (1 - c_prop) ^ 2 / n

  expect_equal(disc$pmse$.null_pmse, expected_null_pmse)
  expect_equal(disc$pmse$.pmse_ratio, disc$pmse$.pmse / expected_null_pmse)

  # split = TRUE computes separate training/testing null pMSEs
  disc_split <- discrimination(ed) |>
      add_propensities(
        recipe = rec,
        spec = glm_mod
      ) |>
      add_pmse() |>
      add_pmse_ratio(method = "logistic")

  expect_equal(nrow(disc_split$pmse), 2)
  expect_true(all(c(".null_pmse", ".pmse_ratio") %in% names(disc_split$pmse)))

})

test_that("add_pmse_ratio method = 'logistic' errors for non-logistic/non-glm discriminators", {

  set.seed(1)

  data <-
    data.frame(
      x = rnorm(n = 200, mean = 0, sd = 1),
      y = rnorm(n = 200, mean = 0, sd = 1)
    )

  postsynth <-
    list(
      synthetic_data = data,
      jth_synthesis_time = data.frame(
        variable = factor(c("x", "y"))
      )
    ) |>
    structure(class = "postsynth")

  ed <- eval_data(conf_data = data, synth_data = postsynth)

  dt_mod <- parsnip::decision_tree() |>
    parsnip::set_mode(mode = "classification") |>
    parsnip::set_engine(engine = "rpart")

  rec <- recipes::recipe(.source_label ~ ., data = discrimination(ed)$combined_data)

  disc <- discrimination(ed) |>
    add_propensities(
      recipe = rec,
      spec = dt_mod
    ) |>
    add_pmse(split = FALSE)


  expect_error(
    add_pmse_ratio(disc, split = FALSE, method = "logistic"),
    regexp = "logistic_reg"
  )

})

test_that("add_pmse_ratio errors for an invalid method", {

  set.seed(1)

  data <-
    data.frame(
      x = rnorm(n = 50, mean = 0, sd = 1),
      y = rnorm(n = 50, mean = 0, sd = 1)
    )

  postsynth <-
    list(
      synthetic_data = data,
      jth_synthesis_time = data.frame(
        variable = factor(c("x", "y"))
      )
    ) |>
    structure(class = "postsynth")

  ed <- eval_data(conf_data = data, synth_data = postsynth)

  dt_mod <- parsnip::decision_tree() |>
    parsnip::set_mode(mode = "classification") |>
    parsnip::set_engine(engine = "rpart")

  rec <- recipes::recipe(.source_label ~ ., data = discrimination(ed)$combined_data)

  disc <- 
  discrimination(ed) |>
    add_propensities(
    recipe = rec,
    spec = dt_mod
  ) |>
  add_pmse(split = FALSE)

  expect_error(add_pmse_ratio(disc, split = FALSE, method = "nonsense"))

})

test_that("add_pmse_ratio is reproducible and plan-independent", {

  skip_if_not_installed("future")

  data <-
    data.frame(
      x = rnorm(n = 200, mean = 0, sd = 1),
      y = rnorm(n = 200, mean = 0, sd = 1)
    )

  postsynth <-
    list(
      synthetic_data = data,
      jth_synthesis_time = data.frame(
        variable = factor(c("x", "y"))
      )
    ) |>
    structure(class = "postsynth")

  ed <- eval_data(conf_data = data, synth_data = postsynth)

  dt_mod <- parsnip::decision_tree() |>
    parsnip::set_mode(mode = "classification") |>
    parsnip::set_engine(engine = "rpart")

  rec <- recipes::recipe(.source_label ~ ., data = discrimination(ed)$combined_data)

  disc <- suppressWarnings(
    discrimination(ed) |>
      add_propensities(
        recipe = rec,
        spec = dt_mod
      ) |>
      add_pmse(split = FALSE)
  )

  # sequential, twice with the same seed
  set.seed(20260730)
  seq_run1 <- add_pmse_ratio(disc, split = FALSE, times = 5)

  set.seed(20260730)
  seq_run2 <- add_pmse_ratio(disc, split = FALSE, times = 5)

  expect_equal(seq_run1$pmse$.null_pmse, seq_run2$pmse$.null_pmse)

  # split = FALSE cannot supply null pMSEs for a split pmse
  disc_split <- suppressWarnings(add_pmse(disc, split = TRUE))
  expect_error(
    add_pmse_ratio(disc_split, split = FALSE, times = 2),
    regexp = "split = TRUE"
  )

  # parallel plan with the same seed must match the sequential result
  future::plan(future::multisession, workers = 2)
  on.exit(future::plan(future::sequential), add = TRUE)

  set.seed(20260730)
  par_run <- add_pmse_ratio(disc, split = FALSE, times = 5)

  expect_equal(seq_run1$pmse$.null_pmse, par_run$pmse$.null_pmse)

})

test_that("add_pmse returns perfect value for identical data without variation " , {
  
  set.seed(1)
  
  data <-
    data.frame(
      x = rep(1, 1000),
      y = rep(1, 1000)
    )
  
  postsynth <-
    list(
      synthetic_data = data,
      jth_synthesis_time = data.frame(
        variable = factor(c("x", "y"))
      )
    ) |>
    structure(class = "postsynth")
  
  ed <- eval_data(conf_data = data, synth_data = postsynth)
  
  dt_mod <- parsnip::decision_tree() |>
    parsnip::set_mode(mode = "classification") |>
    parsnip::set_engine(engine = "rpart")
  
  rec <- recipes::recipe(.source_label ~ ., data = discrimination(ed)$combined_data)
  
  disc <- suppressWarnings(
    discrimination(ed) |>
      add_propensities(
        recipe = rec,
        spec = dt_mod
      ) |>
      add_pmse() |>
      add_pmse_ratio(times = 25)
  )
  
  expect_equal(disc$pmse$.pmse, c(0, 0))
  # permuting labels on constant data can leave tiny rpart floating-point
  # noise rather than an exact 0, unlike the old bootstrap-based null
  expect_equal(disc$pmse$.null_pmse, c(0, 0), tolerance = 1e-3)
  
})
