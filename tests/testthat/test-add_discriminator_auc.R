test_that("add_discriminator_auc returns perfect value for identical data (no split) " , {
  
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
  
  disc <- discrimination(postsynth, data) %>%
    add_propensities(
      recipe = rec,
      spec = logistic_mod
    ) %>%
    add_discriminator_auc(split = FALSE)
  
  expect_equal(disc$discriminator_auc$.estimate, 0.5)
  
})

test_that("add_discriminator_auc returns perfect value for identical data (split) " , {
  
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
      add_discriminator_auc()
  )
  
  expect_equal(disc$discriminator_auc$.estimate, c(0.5, 0.5))
  
})

test_that("add_pmse returns perfect value for seperable data (no split)" , {
  
  set.seed(1)
  
  data <-
    data.frame(
      x = c(rep(1, 500), rep(2, 500)),
      y = c(rep(1, 500), rep(2, 500))
    )
  
  data2 <-
    data.frame(
      x = c(rep(3, 500), rep(4, 500)),
      y = c(rep(3, 500), rep(4, 500))
    )
  
  postsynth <-
    list(
      synthetic_data = data2,
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
    add_discriminator_auc(split = FALSE)
  
  expect_equal(disc$discriminator_auc$.estimate, 1)
  
})
