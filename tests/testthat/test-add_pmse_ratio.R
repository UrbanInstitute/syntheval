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
    ) %>%
    structure(class = "postsynth")
  
  ed <- eval_data(conf_data = data, synth_data = postsynth)
  
  dt_mod <- parsnip::decision_tree() %>%
    parsnip::set_mode(mode = "classification") %>%
    parsnip::set_engine(engine = "rpart")
  
  rec <- recipes::recipe(.source_label ~ ., data = discrimination(ed)$combined_data)
  
  disc <- suppressWarnings(
    discrimination(ed) %>%
      add_propensities(
        recipe = rec,
        spec = dt_mod
      ) 
  )
  
  expect_error(add_pmse_ratio(disc))
  
  disc <- disc %>%
    add_pmse(split = FALSE) %>%
    add_pmse_ratio(split = FALSE, times = 25)
  
  expect_equal(round(disc$pmse$.pmse, digit = 1), 0)
  # this is a bad test but will at least tell us when the code logic changes
  expect_equal(round(disc$pmse$.pmse_ratio, 5), 0.59907)
  
  disc <- disc %>%
    add_pmse() %>%
    add_pmse_ratio(times = 25)
  
  expect_equal(round(disc$pmse$.pmse, digit = 2), c(0, 0))
  # this is a bad test but will at least tell us when the code logic changes
  expect_equal(round(disc$pmse$.pmse_ratio, 5), c(0.28372, 0.62333))
  
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
    ) %>%
    structure(class = "postsynth")

  ed <- eval_data(conf_data = data, synth_data = postsynth)

  dt_mod <- parsnip::decision_tree() %>%
    parsnip::set_mode(mode = "classification") %>%
    parsnip::set_engine(engine = "rpart")

  rec <- recipes::recipe(.source_label ~ ., data = discrimination(ed)$combined_data)

  disc <- suppressWarnings(
    discrimination(ed) %>%
      add_propensities(
        recipe = rec,
        spec = dt_mod
      ) %>%
      add_pmse(split = FALSE)
  )

  # sequential, twice with the same seed
  set.seed(20260730)
  seq_run1 <- add_pmse_ratio(disc, split = FALSE, times = 5)

  set.seed(20260730)
  seq_run2 <- add_pmse_ratio(disc, split = FALSE, times = 5)

  expect_equal(seq_run1$pmse$.null_pmse, seq_run2$pmse$.null_pmse)

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
    ) %>%
    structure(class = "postsynth")
  
  ed <- eval_data(conf_data = data, synth_data = postsynth)
  
  logistic_mod <- parsnip::logistic_reg() %>%
    parsnip::set_mode(mode = "classification") %>%
    parsnip::set_engine(engine = "glm")
  
  rec <- recipes::recipe(.source_label ~ ., data = discrimination(ed)$combined_data)
  
  disc <- suppressWarnings(
    discrimination(ed) %>%
      add_propensities(
        recipe = rec,
        spec = logistic_mod
      ) %>%
      add_pmse() %>%
      add_pmse_ratio(times = 25)
  )
  
  expect_equal(disc$pmse$.pmse, c(0, 0))
  expect_equal(disc$pmse$.null_pmse, c(0, 0))
  expect_equal(disc$pmse$.pmse_ratio, c(NaN, NaN))
  
})
