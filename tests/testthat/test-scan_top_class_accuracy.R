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

test_that("scan_top_class_accuracy basic functionality", {
  res <- scan_top_class_accuracy(toy_scan)

  expect_identical(
    names(res),
    c("source", "target_var", "key_id", "q", "top_class_accuracy")
  )

  synth_res <- res |>
    dplyr::filter(source == "synthetic") |>
    dplyr::arrange(q)

  # Expected per-class accuracies: A all X -> 1; B source top=X (p_source(X)=2/3), conf P_conf(X)=1/2 -> 0.5
  expect_equal(synth_res$top_class_accuracy, c(1, 0.5))

})

test_that("scan_top_class_accuracy includes holdout when supplied", {
  tacc <- scan_top_class_accuracy(toy_scan_holdout) |>
    dplyr::filter(source == "holdout") |>
    dplyr::arrange(q)

  # Holdout: class A all X -> 1; class B both levels present in holdout so top-class accuracy = 1
  expect_equal(tacc$top_class_accuracy, c(1, 1))

})
