
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

test_that("scan_k_anonymity input errors", {
  
  expect_error(scan_k_anonymity("not an attribute_scan object"))
  
})

test_that("scan_k_anonymity basic functionality", {
  
  res <- scan_k_anonymity(toy_scan)
  
  expect_identical(names(res), c("source", "k"))
  expect_identical(res$source, c("confidential", "synthetic"))
  
  # confidential: class A n=3, class B n=2 -> min 2
  # synthetic: class A n=2, class B n=3 -> min 2
  expect_equal(res$k, c(2, 2))
  
})

test_that("scan_k_anonymity matches manual .aggregate_qid computation", {
  
  scan <- attribute_scan(
    eval_data(conf_data = acs_conf, synth_data = acs_lr_synths[[1]]),
    qid_keys = c("county", "gq")
  )
  
  res <- scan_k_anonymity(scan)
  
  manual_conf <- min(
    .aggregate_qid(acs_conf, keys = c("county", "gq")) |>
      dplyr::filter(raw_n > 0) |>
      dplyr::pull(raw_n)
  )
  
  expect_equal(
    res$k[res$source == "confidential"],
    manual_conf
  )
  
})

test_that("scan_k_anonymity omits holdout when eval_data$holdout_data is not supplied", {
  
  res <- scan_k_anonymity(toy_scan)
  
  expect_false("holdout" %in% res$source)
  
})

test_that("scan_k_anonymity includes holdout when eval_data$holdout_data is supplied", {
  
  res <- scan_k_anonymity(toy_scan_holdout)
  
  expect_identical(res$source, c("confidential", "synthetic", "holdout"))
  
  # holdout: class A n=1, class B n=4 -> min 1
  expect_equal(res$k[res$source == "holdout"], 1)
  
})
