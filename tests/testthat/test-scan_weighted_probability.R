
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

test_that("scan_weighted_probability input errors", {
  
  expect_error(scan_weighted_probability("not an attribute_scan object"))
  
})

test_that("scan_weighted_probability basic functionality", {
  
  res <- scan_weighted_probability(toy_scan)
  
  expect_identical(names(res), c("source", "target_var", "weighted_probability"))
  
  # confidential: class A (n=3, max prob 1), class B (n=2, max prob 0.5)
  # weighted = (1 * 3 + 0.5 * 2) / 5 = 0.8
  expect_equal(res$weighted_probability[res$source == "confidential"], 0.8)
  
  # synthetic: class A (n=2, max prob 0.5), class B (n=3, max prob 2/3)
  # weighted = (0.5 * 2 + 2/3 * 3) / 5 = 0.6
  expect_equal(res$weighted_probability[res$source == "synthetic"], 0.6)
  
})

test_that("scan_weighted_probability differs from scan_mean_probability with uneven class sizes", {
  
  res_weighted <- scan_weighted_probability(toy_scan)
  res_mean <- scan_mean_probability(toy_scan)
  
  expect_false(isTRUE(all.equal(
    res_weighted$weighted_probability,
    res_mean$mean_probability
  )))
  
})
