
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

test_that("scan_max_probability input errors", {
  
  expect_error(scan_max_probability("not an attribute_scan object"))
  
})

test_that("scan_max_probability basic functionality", {
  
  res <- scan_max_probability(toy_scan, summarize = FALSE)
  
  expect_identical(
    names(res),
    c("source", "target_var", "key_id", "q", "class_n", "max_probability")
  )
  
  conf_res <- res |>
    dplyr::filter(source == "confidential") |>
    dplyr::arrange(q)
  
  # class A: 3 records, all X -> max prob 1; class B: 2 records, 1 X, 1 Y -> max prob 0.5
  expect_equal(conf_res$class_n, c(3, 2))
  expect_equal(conf_res$max_probability, c(1, 0.5))
  
  synth_res <- res |>
    dplyr::filter(source == "synthetic") |>
    dplyr::arrange(q)
  
  # class A: 2 records, 1 X, 1 Y -> max prob 0.5; class B: 3 records, 2 X, 1 Y -> max prob 2/3
  expect_equal(synth_res$class_n, c(2, 3))
  expect_equal(synth_res$max_probability, c(0.5, 2 / 3))
  
})

test_that("scan_max_probability summarize = TRUE reports the worst-case (maximum) class", {
  
  res <- scan_max_probability(toy_scan)
  
  expect_identical(
    names(res),
    c("source", "target_var", "key_id", "q", "class_n", "max_probability")
  )
  
  # one row per source/target_var
  expect_equal(nrow(res), 2)
  
  conf_res <- res |>
    dplyr::filter(source == "confidential")
  
  # class A (n=3) has max_probability 1, class B (n=2) has max_probability 0.5 -> maximum is class A
  expect_equal(conf_res$q, factor("A", levels = c("A", "B")))
  expect_equal(conf_res$class_n, 3)
  expect_equal(conf_res$max_probability, 1)
  
  synth_res <- res |>
    dplyr::filter(source == "synthetic")
  
  # class A (n=2) has max_probability 0.5, class B (n=3) has max_probability 2/3 -> maximum is class B
  expect_equal(synth_res$q, factor("B", levels = c("A", "B")))
  expect_equal(synth_res$class_n, 3)
  expect_equal(synth_res$max_probability, 2 / 3)
  
})

test_that("scan_max_probability omits holdout when eval_data$holdout_data is not supplied", {
  
  res <- scan_max_probability(toy_scan, summarize = FALSE)
  
  expect_false("holdout" %in% res$source)
  
})

test_that("scan_max_probability includes holdout when eval_data$holdout_data is supplied", {
  
  holdout_res <- scan_max_probability(toy_scan_holdout, summarize = FALSE) |>
    dplyr::filter(source == "holdout") |>
    dplyr::arrange(q)
  
  # class A: 1 record, all X -> max prob 1; class B: 4 records, 2 X, 2 Y -> max prob 0.5
  expect_equal(holdout_res$class_n, c(1, 4))
  expect_equal(holdout_res$max_probability, c(1, 0.5))
  
})
