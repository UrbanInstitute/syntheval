
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

test_that("scan_information_gain input errors", {
  
  expect_error(scan_information_gain("not an attribute_scan object"))
  
})

test_that("scan_information_gain basic functionality", {
  
  res <- scan_information_gain(toy_scan)
  
  expect_identical(names(res), c("source", "target_var", "information_gain"))
  
  # confidential: overall t distribution X = 4/5, Y = 1/5
  # conditional entropy: class A (n=3, entropy 0) + class B (n=2, entropy 1),
  # weighted by class size -> (0 * 3 + 1 * 2) / 5
  conf_overall_entropy <- -(0.8 * log2(0.8) + 0.2 * log2(0.2))
  conf_conditional_entropy <- (0 * 3 + 1 * 2) / 5
  
  expect_equal(
    res$information_gain[res$source == "confidential"],
    conf_overall_entropy - conf_conditional_entropy
  )
  
  # synthetic: overall t distribution X = 3/5, Y = 2/5
  # conditional entropy: class A (n=2, entropy 1) + class B (n=3, entropy_b),
  # weighted by class size
  synth_overall_entropy <- -(0.6 * log2(0.6) + 0.4 * log2(0.4))
  synth_conditional_entropy <- ((1 * 2) + (-(2 / 3 * log2(2 / 3) + 1 / 3 * log2(1 / 3)) * 3)) / 5
  
  expect_equal(
    res$information_gain[res$source == "synthetic"],
    synth_overall_entropy - synth_conditional_entropy
  )
  
})

test_that("scan_information_gain is zero when qid_keys reveal nothing about the target", {
  
  even_conf <- data.frame(
    q = factor(c("A", "A", "B", "B"), levels = c("A", "B")),
    t = factor(c("X", "Y", "X", "Y"), levels = c("X", "Y"))
  )
  
  scan <- attribute_scan(
    eval_data(conf_data = even_conf, synth_data = even_conf),
    qid_keys = "q",
    target_keys = "t"
  )
  
  res <- scan_information_gain(scan)
  
  expect_equal(res$information_gain[res$source == "confidential"], 0)
  
})
