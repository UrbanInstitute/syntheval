
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

test_that("scan_l_diversity input errors", {
  
  expect_error(scan_l_diversity("not an attribute_scan object"))
  
})

test_that("scan_l_diversity basic functionality", {
  
  res <- scan_l_diversity(toy_scan)
  
  expect_identical(
    names(res),
    c("source", "target_var", "key_id", "q", "l_diversity")
  )
  
  conf_res <- res |>
    dplyr::filter(source == "confidential") |>
    dplyr::arrange(q)
  
  # class A: all X -> 1 distinct level; class B: 1 X, 1 Y -> 2 distinct levels
  expect_equal(conf_res$l_diversity, c(1, 2))
  
  synth_res <- res |>
    dplyr::filter(source == "synthetic") |>
    dplyr::arrange(q)
  
  # class A: 1 X, 1 Y -> 2 distinct levels; class B: 2 X, 1 Y -> 2 distinct levels
  expect_equal(synth_res$l_diversity, c(2, 2))
  
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
  
  res <- scan_l_diversity(scan)
  
  # confidential class A only ever observes "X" (never "Y"), so l-diversity is 1
  expect_equal(res$l_diversity[res$source == "confidential"], 1)
  expect_equal(res$l_diversity[res$source == "synthetic"], 2)
  
})
