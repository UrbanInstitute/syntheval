test_that("add_pmse returns perfect value for identical data (no split)" , {
  
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
  
  logistic_mod <- parsnip::logistic_reg() %>%
    parsnip::set_mode(mode = "classification") %>%
    parsnip::set_engine(engine = "glm")
  
  rec <- recipes::recipe(.source_label ~ ., data = discrimination(postsynth, data)$combined_data)
  
  disc <- suppressWarnings(
    discrimination(postsynth, data) %>%
      add_propensities(
        recipe = rec,
        spec = logistic_mod
      ) 
  )
  
  expect_error(add_pmse_ratio(disc))
  
  disc <- disc %>%
    add_pmse(split = FALSE) %>%
    add_pmse_ratio(times = 25)
  
  expect_equal(round(disc$pmse$.pmse, digit = 3), 0)
  
})

test_that("add_pmse returns perfect value for identical data (split) " , {
  
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
