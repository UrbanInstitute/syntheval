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

test_that("scan_confidence basic functionality", {
  res <- scan_confidence(toy_scan)

  expect_identical(names(res), c("source", "target_var", "confidence"))

  synth_res <- res |>
    dplyr::filter(source == "synthetic")

  # Confidence = weighted avg of per-class max probs: class A max=0.5 (3 records), class B max=2/3 (2 records) -> 
  # (0.5 * 3 + 2 / 3 * 2) / 5 = 0.5666667
  expect_equal(as.numeric(synth_res$confidence), 0.5666666666666667, tolerance = 1e-8)

})

test_that("scan_confidence includes holdout when supplied", {
  conf <- scan_confidence(toy_scan_holdout) |>
    dplyr::filter(source == "holdout")

  # Holdout confidence = weighted avg: class A max=1.0 (3 records), class B max=0.5 (2 records) -> (1*3+0.5*2)/5 = 0.8
  expect_equal(as.numeric(conf$confidence), 0.8)

})
