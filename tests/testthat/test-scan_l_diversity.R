
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

test_that("scan_l_diversity input errors", {
  
  expect_error(scan_l_diversity("not an attribute_scan object"))
  
})

test_that("scan_l_diversity basic functionality", {
  
  res <- scan_l_diversity(toy_scan, summarize = FALSE)
  
  expect_identical(
    names(res),
    c("source", "target_var", "key_id", "q", "class_n", "l_diversity")
  )
  
  conf_res <- res |>
    dplyr::filter(source == "confidential") |>
    dplyr::arrange(q)
  
  # class A: 3 records; class B: 2 records
  expect_equal(conf_res$class_n, c(3, 2))
  
  # class A: all X -> 1 distinct level; class B: 1 X, 1 Y -> 2 distinct levels
  expect_equal(conf_res$l_diversity, c(1, 2))
  
  synth_res <- res |>
    dplyr::filter(source == "synthetic") |>
    dplyr::arrange(q)
  
  # class A: 2 records; class B: 3 records
  expect_equal(synth_res$class_n, c(2, 3))
  
  # class A: 1 X, 1 Y -> 2 distinct levels; class B: 2 X, 1 Y -> 2 distinct levels
  expect_equal(synth_res$l_diversity, c(2, 2))
  
})

test_that("scan_l_diversity summarize = TRUE reports the worst-case (minimum) class", {
  
  res <- scan_l_diversity(toy_scan)
  
  expect_identical(
    names(res),
    c("source", "target_var", "key_id", "q", "class_n", "l_diversity")
  )
  
  # one row per source/target_var
  expect_equal(nrow(res), 2)
  
  conf_res <- res |>
    dplyr::filter(source == "confidential")
  
  # class A (n=3) has l_diversity 1, class B (n=2) has l_diversity 2 -> minimum is class A
  expect_equal(conf_res$q, factor("A", levels = c("A", "B")))
  expect_equal(conf_res$class_n, 3)
  expect_equal(conf_res$l_diversity, 1)
  
  synth_res <- res |>
    dplyr::filter(source == "synthetic")
  
  # both synthetic classes tie at l_diversity 2; the minimum value is unambiguous
  expect_equal(synth_res$l_diversity, 2)
  
})

test_that("scan_l_diversity ignores unobserved target levels", {
  
  degenerate_conf <- data.frame(
    q = factor(c("A", "A"), levels = c("A")),
    t = factor(c("X", "X"), levels = c("X", "Y"))
  )
  
  degenerate_synth <- data.frame(
    q = factor(c("A", "A"), levels = c("A")),
    t = factor(c("X", "Y"), levels = c("X", "Y"))
  )
  
  scan <- attribute_scan(
    eval_data(conf_data = degenerate_conf, synth_data = degenerate_synth),
    qid_keys = "q",
    target_keys = "t"
  )
  
  res <- scan_l_diversity(scan, summarize = FALSE)
  
  # confidential class A only ever observes "X" (never "Y"), so l-diversity is 1
  expect_equal(res$l_diversity[res$source == "confidential"], 1)
  expect_equal(res$l_diversity[res$source == "synthetic"], 2)
  
  expect_equal(res$class_n[res$source == "confidential"], 2)
  expect_equal(res$class_n[res$source == "synthetic"], 2)
  
})

test_that("scan_l_diversity omits holdout when eval_data$holdout_data is not supplied", {
  
  res <- scan_l_diversity(toy_scan, summarize = FALSE)
  
  expect_false("holdout" %in% res$source)
  
})

test_that("scan_l_diversity includes holdout when eval_data$holdout_data is supplied", {
  
  res <- scan_l_diversity(toy_scan_holdout, summarize = FALSE) |>
    dplyr::filter(source == "holdout") |>
    dplyr::arrange(q)
  
  # class A: 1 record; class B: 4 records
  expect_equal(res$class_n, c(1, 4))
  
  # class A: only X observed -> 1 distinct level; class B: 2 X, 2 Y -> 2 distinct levels
  expect_equal(res$l_diversity, c(1, 2))
  
})
