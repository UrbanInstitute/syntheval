
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

test_that("scan_entropy input errors", {
  
  expect_error(scan_entropy("not an attribute_scan object"))
  
})

test_that("scan_entropy basic functionality", {
  
  res <- scan_entropy(toy_scan)
  
  expect_identical(
    names(res),
    c("source", "target_var", "key_id", "q", "value")
  )
  
  expect_true(all(res$value >= 0))
  expect_true(all(res$value <= log2(2)))
  
  conf_res <- res |>
    dplyr::filter(source == "confidential") |>
    dplyr::arrange(q)
  
  # class A: all X -> entropy 0; class B: 1 X, 1 Y -> entropy 1
  expect_equal(conf_res$value, c(0, 1))
  
  synth_res <- res |>
    dplyr::filter(source == "synthetic") |>
    dplyr::arrange(q)
  
  # class A: 1 X, 1 Y -> entropy 1
  # class B: 2 X, 1 Y -> entropy = -(2/3 log2(2/3) + 1/3 log2(1/3))
  expected_b <- -(2 / 3 * log2(2 / 3) + 1 / 3 * log2(1 / 3))
  expect_equal(synth_res$value, c(1, expected_b))
  
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
  
  res <- scan_entropy(scan)
  
  expect_equal(res$value[res$source == "confidential"], 0)
  expect_equal(res$value[res$source == "synthetic"], 1)
  
})
