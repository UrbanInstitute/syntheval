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

test_that("scan_entropy_ratio basic functionality", {
  res <- scan_entropy_ratio(toy_scan_holdout)

  expect_identical(
    names(res),
    c("target_var", "synthetic", "holdout", "entropy_ratio")
  )

  # from test-scan_expected_entropy.R: synthetic expected_entropy = 0.9673183333333333, holdout = 0.4
  expect_equal(as.numeric(res$synthetic), 0.9673183333333333, tolerance = 1e-8)
  expect_equal(as.numeric(res$holdout), 0.4)
  expect_equal(as.numeric(res$entropy_ratio), 0.9673183333333333 / 0.4, tolerance = 1e-8)

})

test_that("scan_entropy_ratio errors without holdout data", {
  expect_error(scan_entropy_ratio(toy_scan_no_holdout))
})
