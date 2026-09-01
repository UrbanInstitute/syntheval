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

test_that("scan_predictability_ratio basic functionality", {
  res <- scan_predictability_ratio(toy_scan_holdout)

  expect_identical(
    names(res),
    c("target_var", "synthetic", "holdout", "predictability_ratio")
  )

  # synthetic predictability = 4 / 5, holdout predictability = 1 (from test-scan_predictability.R)
  expect_equal(res$synthetic, 4 / 5)
  expect_equal(res$holdout, 1)
  expect_equal(res$predictability_ratio, (4 / 5) / 1)

})

test_that("scan_predictability_ratio errors without holdout data", {
  expect_error(scan_predictability_ratio(toy_scan_no_holdout))
})
