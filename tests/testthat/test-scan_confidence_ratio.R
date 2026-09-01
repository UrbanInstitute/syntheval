toy_conf <- data.frame(
  q = factor(c("A", "A", "A", "B", "B"), levels = c("A", "B")),
  t = factor(c("X", "X", "X", "Y", "X"), levels = c("X", "Y"))
)

toy_synth <- data.frame(
  q = factor(c("A", "A", "B", "B", "B"), levels = c("A", "B")),
  t = factor(c("X", "Y", "X", "X", "Y"), levels = c("X", "Y"))
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

toy_scan_no_holdout <- attribute_scan(
  eval_data(conf_data = toy_conf, synth_data = toy_synth),
  qid_keys = "q",
  target_keys = "t"
)

test_that("scan_confidence_ratio basic functionality", {
  res <- scan_confidence_ratio(toy_scan_holdout)

  expect_identical(
    names(res),
    c("target_var", "synthetic", "holdout", "confidence_ratio")
  )

  # from test-scan_confidence.R: synthetic confidence = 0.5666666666666667, holdout confidence = 0.8
  expect_equal(as.numeric(res$synthetic), 0.5666666666666667, tolerance = 1e-8)
  expect_equal(as.numeric(res$holdout), 0.8)
  expect_equal(as.numeric(res$confidence_ratio), 0.5666666666666667 / 0.8, tolerance = 1e-8)

})

test_that("scan_confidence_ratio errors without holdout data", {
  expect_error(scan_confidence_ratio(toy_scan_no_holdout))
})
