
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


test_that("scan_truth_probability input errors", {
  expect_error(scan_truth_probability("not an attribute_scan object"))
})


test_that("scan_truth_probability basic functionality", {
  res <- scan_truth_probability(toy_scan)

  expect_identical(
    names(res),
    c("source", "target_var", "key_id", "q", "truth_probability")
  )

  synth_res <- res |>
    dplyr::filter(source == "synthetic") |>
    dplyr::arrange(q)

  # for this toy example both classes yield expected truth_probability 0.5
  expect_equal(synth_res$truth_probability, c(0.5, 0.5))

})


test_that("scan_truth_probability includes holdout when supplied", {
  res <- scan_truth_probability(toy_scan_holdout)

  expect_true("holdout" %in% res$source)

  holdout_res <- res |>
    dplyr::filter(source == "holdout") |>
    dplyr::arrange(q)

  # Holdout truth_probability per class: class A conf all X & source assigns p=1 -> 1; class B conf mix leads to 0.5
  expect_equal(holdout_res$truth_probability, c(1, 0.5))

})
