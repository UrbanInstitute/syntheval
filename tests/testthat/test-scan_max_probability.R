
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

test_that("scan_max_probability input errors", {
  
  expect_error(scan_max_probability("not an attribute_scan object"))
  
})

test_that("scan_max_probability basic functionality", {
  
  res <- scan_max_probability(toy_scan)
  
  expect_identical(
    names(res),
    c("source", "target_var", "key_id", "q", "max_probability")
  )
  
  conf_res <- res |>
    dplyr::filter(source == "confidential") |>
    dplyr::arrange(q)
  
  # class A: all X -> max prob 1; class B: 1 X, 1 Y -> max prob 0.5
  expect_equal(conf_res$max_probability, c(1, 0.5))
  
  synth_res <- res |>
    dplyr::filter(source == "synthetic") |>
    dplyr::arrange(q)
  
  # class A: 1 X, 1 Y -> max prob 0.5; class B: 2 X, 1 Y -> max prob 2/3
  expect_equal(synth_res$max_probability, c(0.5, 2 / 3))
  
})
