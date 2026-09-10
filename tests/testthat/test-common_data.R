
test_that("common_data() restricts to common variables and combines rows", {
  
  conf <- data.frame(x = 1:5, y = 1:5, z = 1:5)
  synth <- data.frame(x = 1:5, y = 1:5)
  
  ed <- eval_data(conf_data = conf, synth_data = synth)
  
  expect_message(
    common <- common_data(ed)
  )
  
  expect_identical(names(common), c("x", "y"))
  expect_equal(nrow(common), nrow(conf) + nrow(synth))
  
})

test_that("common_data() includes holdout data when supplied", {
  
  conf <- data.frame(x = 1:5, y = 1:5)
  synth <- data.frame(x = 1:5, y = 1:5)
  holdout <- data.frame(x = 1:5, y = 1:5, w = 1:5)
  
  ed <- eval_data(conf_data = conf, synth_data = synth, holdout_data = holdout)
  
  expect_message(
    common <- common_data(ed)
  )
  
  expect_identical(names(common), c("x", "y"))
  expect_equal(nrow(common), nrow(conf) + nrow(synth) + nrow(holdout))
  
})

test_that("common_data() unions factor levels across sources", {
  
  conf <- data.frame(g = factor(c("A", "A"), levels = c("A", "B")))
  synth <- data.frame(g = factor(c("B", "B"), levels = c("A", "B")))
  
  ed <- eval_data(conf_data = conf, synth_data = synth)
  
  common <- common_data(ed)
  
  expect_identical(levels(common$g), c("A", "B"))
  
})

test_that("common_data() uses the first synthetic replicate with a message", {
  
  conf <- data.frame(x = 1:5, y = 1:5)
  synth <- data.frame(x = 1:5, y = 1:5)
  
  ed <- eval_data(conf_data = conf, synth_data = list(synth, synth))
  
  expect_message(
    common <- common_data(ed)
  )
  
  expect_equal(nrow(common), nrow(conf) + nrow(synth))
  
})

test_that("common_data() input errors", {
  
  expect_error(
    common_data("not an eval_data object")
  )
  
})
