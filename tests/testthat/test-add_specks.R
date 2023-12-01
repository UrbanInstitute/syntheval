test_that("add_specks returns perfect value for identical data (no split) " , {
  
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
  
  disc <- discrimination(postsynth, data) |>
    add_propensities(
      recipe = rec,
      spec = logistic_mod
    ) |>
    add_specks(split = FALSE)
  
  expect_equal(disc$specks$.specks, 0)
  
})

test_that("add_specks returns perfect value for identical data (split) " , {
  
  set.seed(1)
  
  data <-
    data.frame(
      x = rep(1, times = 1000),
      y = rep(1, times = 1000)
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
    discrimination(postsynth, data) |>
      add_propensities(
        recipe = rec,
        spec = logistic_mod
      ) |>
      add_specks()
  )
    
  expect_equal(disc$specks$.specks, c(0, 0))
  
})
