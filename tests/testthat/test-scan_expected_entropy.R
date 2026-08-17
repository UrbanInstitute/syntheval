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

test_that("scan_expected_entropy basic functionality", {
  res <- scan_expected_entropy(toy_scan)

  expect_identical(names(res), c("source", "target_var", "expected_entropy"))

  synth_res <- res |>
    dplyr::filter(source == "synthetic")

  # Expected entropy = population-weighted avg: class A H=1.0 (3 records), class B H=0.9182958 (2 records) 
  # -> (1 * 3 + 0.9182958 * 2) / 5 = 0.96731833
  expect_equal(as.numeric(synth_res$expected_entropy), 0.9673183333333333, tolerance = 1e-8)

})

test_that("scan_expected_entropy includes holdout when supplied", {
  eent <- scan_expected_entropy(toy_scan_holdout) |>
    dplyr::filter(source == "holdout")

  # Holdout expected_entropy = (0 * 3 + 1 * 2) / 5 = 0.4
  expect_equal(as.numeric(eent$expected_entropy), 0.4)

})
