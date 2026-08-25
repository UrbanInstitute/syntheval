
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

test_that("scan_entropy input errors", {
  
  expect_error(scan_entropy("not an attribute_scan object"))
  
})

test_that("scan_entropy basic functionality", {
  
  res <- scan_entropy(toy_scan, summarize = FALSE)
  
  expect_identical(
    names(res),
    c("source", "target_var", "key_id", "q", "class_n", "entropy", "max_entropy")
  )
  
  expect_true(all(res$entropy >= 0))
  expect_true(all(res$entropy <= log2(2)))
  expect_equal(res$max_entropy, rep(log2(2), nrow(res)))
  
  conf_res <- res |>
    dplyr::filter(source == "confidential") |>
    dplyr::arrange(q)
  
  # class A: 3 records, all X -> entropy 0; class B: 2 records, 1 X, 1 Y -> entropy 1
  expect_equal(conf_res$class_n, c(3, 2))
  expect_equal(conf_res$entropy, c(0, 1))
  
  synth_res <- res |>
    dplyr::filter(source == "synthetic") |>
    dplyr::arrange(q)
  
  # class A: 2 records, 1 X, 1 Y -> entropy 1
  # class B: 3 records, 2 X, 1 Y -> entropy = -(2/3 log2(2/3) + 1/3 log2(1/3))
  expected_b <- -(2 / 3 * log2(2 / 3) + 1 / 3 * log2(1 / 3))
  expect_equal(synth_res$class_n, c(2, 3))
  expect_equal(synth_res$entropy, c(1, expected_b))
  
})

test_that("scan_entropy summarize = TRUE reports the worst-case (minimum) class", {
  
  res <- scan_entropy(toy_scan)
  
  expect_identical(
    names(res),
    c("source", "target_var", "key_id", "q", "class_n", "entropy", "max_entropy")
  )
  
  # one row per source/target_var
  expect_equal(nrow(res), 2)
  
  conf_res <- res |>
    dplyr::filter(source == "confidential")
  
  # class A (n=3) has entropy 0, class B (n=2) has entropy 1 -> minimum is class A
  expect_equal(conf_res$q, factor("A", levels = c("A", "B")))
  expect_equal(conf_res$class_n, 3)
  expect_equal(conf_res$entropy, 0)
  
  synth_res <- res |>
    dplyr::filter(source == "synthetic")
  
  # class A (n=2) has entropy 1, class B (n=3) has entropy ~0.9183 -> minimum is class B
  expected_b <- -(2 / 3 * log2(2 / 3) + 1 / 3 * log2(1 / 3))
  expect_equal(synth_res$q, factor("B", levels = c("A", "B")))
  expect_equal(synth_res$class_n, 3)
  expect_equal(synth_res$entropy, expected_b)
  
})

test_that("scan_entropy handles perfectly uniform and degenerate cases", {
  
  degenerate_conf <- data.frame(
    q = factor(c("A", "A"), levels = c("A")),
    t = factor(c("X", "X"), levels = c("X", "Y"))
  )
  
  uniform_synth <- data.frame(
    q = factor(c("A", "A"), levels = c("A")),
    t = factor(c("X", "Y"), levels = c("X", "Y"))
  )
  
  scan <- attribute_scan(
    eval_data(conf_data = degenerate_conf, synth_data = uniform_synth),
    qid_keys = "q",
    target_keys = "t"
  )
  
  res <- scan_entropy(scan, summarize = FALSE)
  
  expect_equal(res$entropy[res$source == "confidential"], 0)
  expect_equal(res$entropy[res$source == "synthetic"], 1)
  
})

test_that("scan_entropy omits holdout when eval_data$holdout_data is not supplied", {
  
  res <- scan_entropy(toy_scan, summarize = FALSE)
  
  expect_false("holdout" %in% res$source)
  
})

test_that("scan_entropy includes holdout when eval_data$holdout_data is supplied", {
  
  res <- scan_entropy(toy_scan_holdout, summarize = FALSE) |>
    dplyr::filter(source == "holdout") |>
    dplyr::arrange(q)
  
  # class A: all X -> entropy 0; class B: 2 X, 2 Y -> entropy 1
  expect_equal(res$entropy, c(0, 1))
  
})
