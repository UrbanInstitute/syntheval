test_that("add_specks returns perfect value for identical data (no split) " , {
  
  set.seed(1)
  
  data <-
    data.frame(
      x = rnorm(n = 1000),
      y = rnorm(n = 1000)
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
  
  disc <- discrimination(ed) %>%
    add_propensities(
      recipe = rec,
      spec = logistic_mod
    ) %>%
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
      add_specks()
  )
    
  expect_equal(disc$specks$.specks, c(0, 0))
  
})

test_that("add_specks returns 1 for perfectly different data (no split) " , {
  
  data <-
    data.frame(
      x = rep(c(1, 2), 500),
      y = rep(c(1, 2), 500)
    )
  
  data2 <-
    data.frame(
      x = rep(c(3, 4), 500),
      y = rep(c(3, 4), 500)
    )
  
  postsynth <-
    list(
      synthetic_data = data2,
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
      add_specks(split = FALSE)
  )
  
  expect_equal(round(disc$specks$.specks, 4), 1)
  
})

test_that("add_specks returns 1 for perfectly different data (split) " , {
  
  data <-
    data.frame(
      x = rep(c(1, 2), 500),
      y = rep(c(1, 2), 500)
    )
  
  data2 <-
    data.frame(
      x = rep(c(3, 4), 500),
      y = rep(c(3, 4), 500)
    )
  
  postsynth <-
    list(
      synthetic_data = data2,
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
      add_specks()
  )
  
  expect_equal(round(disc$specks$.specks, 4), c(1, 1))
  
})

test_that("add_specks works with grouping variable", {
  
  set.seed(1)
  
  # Create data with a grouping variable
  data <- data.frame(
    x = rnorm(n = 1000, mean = 0, sd = 1),
    y = rnorm(n = 1000, mean = 0, sd = 1),
    group = rep(c("A", "B"), each = 500)
  )
  
  postsynth <-
    list(
      synthetic_data = data,
      jth_synthesis_time = data.frame(
        variable = factor(c("x", "y", "group"))
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
      add_specks(split = FALSE, group_by_q = "group")
  )
  
  # Check structure
  expect_equal(nrow(disc$specks), 2)  # One row per group
  expect_true(".group" %in% names(disc$specks))
  expect_true(".specks" %in% names(disc$specks))
  expect_true(is.factor(disc$specks$.group))
  expect_equal(levels(disc$specks$.group), c("A", "B"))
  
  # Check that SPECKS values are numeric and in [0, 1]
  expect_true(all(is.numeric(disc$specks$.specks)))
  expect_true(all(disc$specks$.specks >= 0))
  expect_true(all(disc$specks$.specks <= 1))
  
})

test_that("add_specks with grouping and split works correctly", {
  
  set.seed(1)
  
  data <- data.frame(
    x = rnorm(n = 1000, mean = 0, sd = 1),
    y = rnorm(n = 1000, mean = 0, sd = 1),
    group = rep(c("A", "B"), each = 500)
  )
  
  postsynth <-
    list(
      synthetic_data = data,
      jth_synthesis_time = data.frame(
        variable = factor(c("x", "y", "group"))
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
      add_specks(split = TRUE, group_by_q = "group")
  )
  
  # Check structure: 2 groups × 2 splits = 4 rows
  expect_equal(nrow(disc$specks), 4)
  expect_true(".group" %in% names(disc$specks))
  expect_true(".source" %in% names(disc$specks))
  expect_true(".specks" %in% names(disc$specks))
  expect_true(is.factor(disc$specks$.group))
  expect_equal(levels(disc$specks$.group), c("A", "B"))
  expect_true(is.factor(disc$specks$.source))
  expect_equal(levels(disc$specks$.source), c("training", "testing"))
  
})

