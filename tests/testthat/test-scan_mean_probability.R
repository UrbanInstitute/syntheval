
toy_conf <- data.frame(
  q = factor(c("A", "A", "A", "B", "B"), levels = c("A", "B")),
  t = factor(c("X", "X", "X", "Y", "X"), levels = c("X", "Y"))
)

toy_synth <- data.frame(
  q = factor(c("A", "A", "B", "B", "B"), levels = c("A", "B")),
  t = factor(c("X", "Y", "X", "X", "Y"), levels = c("X", "Y"))
)

toy_scan <- attribute_scan(
  eval_data(conf_data = toy_conf, synth_data = toy_synth),
  qid_keys = "q",
  target_keys = "t"
)

toy_holdout <- data.frame(
  q = factor(c("A", "B", "B", "B", "B"), levels = c("A", "B")),
  t = factor(c("X", "X", "X", "Y", "Y"), levels = c("X", "Y"))
)

toy_scan_holdout <- attribute_scan(
  eval_data(conf_data = toy_conf, synth_data = toy_synth, holdout_data = toy_holdout),
  qid_keys = "q",
  target_keys = "t"
)

test_that("scan_mean_probability input errors", {
  
  expect_error(scan_mean_probability("not an attribute_scan object"))
  
})

test_that("scan_mean_probability basic functionality", {
  
  res <- scan_mean_probability(toy_scan)
  
  expect_identical(names(res), c("source", "target_var", "mean_probability"))
  
  # confidential: mean(1, 0.5) = 0.75
  expect_equal(res$mean_probability[res$source == "confidential"], 0.75)
  
  # synthetic: mean(0.5, 2/3) = 7/12
  expect_equal(res$mean_probability[res$source == "synthetic"], mean(c(0.5, 2 / 3)))
  
})

test_that("scan_mean_probability omits holdout when eval_data$holdout_data is not supplied", {
  
  res <- scan_mean_probability(toy_scan)
  
  expect_false("holdout" %in% res$source)
  
})

test_that("scan_mean_probability includes holdout when eval_data$holdout_data is supplied", {
  
  res <- scan_mean_probability(toy_scan_holdout)
  
  # holdout: mean(1, 0.5) = 0.75 (class A max prob 1, class B max prob 0.5)
  expect_equal(res$mean_probability[res$source == "holdout"], 0.75)
  
})
