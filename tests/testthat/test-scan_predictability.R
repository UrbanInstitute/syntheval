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

test_that("scan_predictability basic functionality", {
  res <- scan_predictability(toy_scan)

  expect_identical(names(res), c("source", "target_var", "predictability"))

  synth_res <- res |>
    dplyr::filter(source == "synthetic")

  # Predictability = population-weighted accuracy: 4 correct of 5 records (A:3 correct, B:1 of 2) -> 4/5
  expect_equal(synth_res$predictability, 4 / 5)

})

test_that("scan_predictability includes holdout when supplied", {
  pred <- scan_predictability(toy_scan_holdout) |>
    dplyr::filter(source == "holdout")

  # Holdout predictability = 1 (all 5 records predicted correctly)
  expect_equal(pred$predictability, 1)

})
