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
  
  dt_mod <- parsnip::decision_tree() %>%
    parsnip::set_mode(mode = "classification") %>%
    parsnip::set_engine(engine = "rpart")
  
  rec <- recipes::recipe(.source_label ~ ., data = discrimination(postsynth, data)$combined_data)
  
  disc <- suppressWarnings(
    discrimination(postsynth, data) %>%
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
  expect_equal(round(disc$pmse$.pmse_ratio, 5), 0.56812)
  
  disc <- disc %>%
    add_pmse() %>%
    add_pmse_ratio(times = 25)
  
  expect_equal(round(disc$pmse$.pmse, digit = 2), c(0, 0))
  # this is a bad test but will at least tell us when the code logic changes
  expect_equal(round(disc$pmse$.pmse_ratio, 5), c(0.27469, 0.67145))
  
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
  
  logistic_mod <- parsnip::logistic_reg() %>%
    parsnip::set_mode(mode = "classification") %>%
    parsnip::set_engine(engine = "glm")
  
  rec <- recipes::recipe(.source_label ~ ., data = discrimination(postsynth, data)$combined_data)
  
  disc <- suppressWarnings(
    discrimination(postsynth, data) %>%
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
