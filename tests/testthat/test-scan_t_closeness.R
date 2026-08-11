
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

test_that("scan_t_closeness input errors", {
  
  expect_error(scan_t_closeness("not an attribute_scan object"))
  
  expect_error(scan_t_closeness(toy_scan, metric = "not a metric"))
  
})

test_that("scan_t_closeness basic functionality with default (linf) metric", {
  
  res <- scan_t_closeness(toy_scan)
  
  expect_identical(
    names(res),
    c("source", "target_var", "key_id", "q", "t_closeness")
  )
  
  conf_res <- res |>
    dplyr::filter(source == "confidential") |>
    dplyr::arrange(q)
  
  # overall confidential distribution: X = 4/5, Y = 1/5
  # class A: X = 1, Y = 0 -> max(|1 - 0.8|, |0 - 0.2|) = 0.2
  # class B: X = 0.5, Y = 0.5 -> max(|0.5 - 0.8|, |0.5 - 0.2|) = 0.3
  expect_equal(conf_res$t_closeness, c(0.2, 0.3))
  
})

test_that("scan_t_closeness supports l1 and l2 metrics", {
  
  conf_res_l1 <- scan_t_closeness(toy_scan, metric = "l1") |>
    dplyr::filter(source == "confidential") |>
    dplyr::arrange(q)
  
  # class A: |1 - 0.8| + |0 - 0.2| = 0.4; class B: |0.5-0.8| + |0.5-0.2| = 0.6
  expect_equal(conf_res_l1$t_closeness, c(0.4, 0.6))
  
  conf_res_l2 <- scan_t_closeness(toy_scan, metric = "l2") |>
    dplyr::filter(source == "confidential") |>
    dplyr::arrange(q)
  
  expect_equal(
    conf_res_l2$t_closeness,
    c(sqrt(0.2 ^ 2 + 0.2 ^ 2), sqrt(0.3 ^ 2 + 0.3 ^ 2))
  )
  
})

test_that("scan_t_closeness is zero when a class matches the overall distribution", {
  
  even_conf <- data.frame(
    q = factor(c("A", "A", "B", "B"), levels = c("A", "B")),
    t = factor(c("X", "Y", "X", "Y"), levels = c("X", "Y"))
  )
  
  scan <- attribute_scan(
    eval_data(conf_data = even_conf, synth_data = even_conf),
    qid_keys = "q",
    target_keys = "t"
  )
  
  res <- scan_t_closeness(scan)
  
  expect_equal(res$t_closeness[res$source == "confidential"], c(0, 0))
  
})
