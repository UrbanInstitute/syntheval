test_that("add_propensities() errors when passed non-discrimination " , {
  
  logistic_mod <- parsnip::logistic_reg() %>%
    parsnip::set_mode(mode = "classification") %>%
    parsnip::set_engine(engine = "glm")
  
  expect_error(
    add_propensities(
      discrimination = penguins_conf,
      spec = logistic_mod
    )
  )
  
})


test_that("Three recipe methods return identical results" , {

  logistic_mod <- parsnip::logistic_reg() %>%
    parsnip::set_mode(mode = "classification") %>%
    parsnip::set_engine(engine = "glm")
  
  rec <- recipes::recipe(.source_label ~ ., data = discrimination(penguins_postsynth, penguins_conf)$combined_data)
  
  # recipe and formula
  set.seed(1)
  approach_custom <- discrimination(postsynth = penguins_postsynth, data = penguins_conf) %>%
    add_propensities(
      recipe = rec,
      spec = logistic_mod
    )
  
  # no recipe, no formula
  set.seed(1)
  approach_default <- suppressMessages(
    discrimination(postsynth = penguins_postsynth, data = penguins_conf) %>%
      add_propensities(
        spec = logistic_mod
      )
  )
  
  # formula and no recipe
  set.seed(1)
  approach_formula <- discrimination(postsynth = penguins_postsynth, data = penguins_conf) %>%
    add_propensities(
      spec = logistic_mod,
      formula = .source_label ~ .
    )
  
  expect_equal(approach_custom, approach_default)
  expect_equal(approach_custom, approach_formula)
  
})
