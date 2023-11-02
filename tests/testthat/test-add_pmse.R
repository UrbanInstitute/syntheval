test_that("add_pmse returns perfect value for identical data " , {
  
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
    add_pmse()
  
  expect_equal(disc$pmse, 0)
  
})
