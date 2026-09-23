conf <- data.frame(
  a = c(1, NA, 3, 4),
  b = c(NA, NA, "x", "y"),
  c = c(1, 2, 3, 4)
)

synth_same <- data.frame(
  a = c(1, NA, 3, 4),
  b = c(NA, NA, "x", "y"),
  c = c(1, 2, 3, 4)
)

test_that("util_na_amount matches when conf and synth are identical", {
  
  ed <- eval_data(conf_data = conf, synth_data = synth_same)
  
  result <- util_na_amount(ed)
  
  expect_equal(
    result$na_prop[result$source == "original"],
    result$na_prop[result$source == "synthetic"]
  )
  
})

synth_diff <- data.frame(
  a = c(1, 2, 3, 4),
  b = c(NA, NA, NA, "y"),
  c = c(1, 2, 3, 4)
)

test_that("util_na_amount detects differences in missingness between conf and synth", {
  
  ed <- eval_data(conf_data = conf, synth_data = synth_diff)
  
  result <- util_na_amount(ed)
  
  expect_equal(
    result$na_prop[result$variable == "a" & result$source == "original"],
    0.25
  )
  expect_equal(
    result$na_prop[result$variable == "a" & result$source == "synthetic"],
    0
  )
  expect_equal(
    result$na_prop[result$variable == "b" & result$source == "synthetic"],
    0.75
  )
  
})

holdout <- data.frame(
  a = c(1, 2, NA, 4),
  b = c("x", "x", "x", "y"),
  c = c(1, 2, 3, 4)
)

test_that("util_na_amount includes holdout rows when supplied", {
  
  ed <- eval_data(conf_data = conf, synth_data = synth_same, holdout_data = holdout)
  
  result <- util_na_amount(ed)
  
  expect_setequal(
    unique(result$source),
    c("original", "synthetic", "holdout")
  )
  
})

data_sentinel <- data.frame(
  a = c(1, -99, 3, NA),
  c = c(1, 2, 3, 4)
)

test_that("util_na_amount respects a custom na_values sentinel", {
  
  ed <- eval_data(conf_data = data_sentinel, synth_data = data_sentinel)
  
  result <- util_na_amount(ed, na_values = -99)
  
  expect_equal(
    result$na_prop[result$variable == "a" & result$source == "original"],
    0.5
  )
  expect_equal(
    result$na_prop[result$variable == "a" & result$source == "synthetic"],
    0.5
  )
  
})
