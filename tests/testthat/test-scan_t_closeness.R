
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

test_that("scan_t_closeness input errors", {
  
  expect_error(scan_t_closeness("not an attribute_scan object"))
  
  expect_error(scan_t_closeness(toy_scan, metric = "not a metric"))
  
})

test_that("scan_t_closeness basic functionality with default (linf) metric", {
  
  res <- scan_t_closeness(toy_scan, summarize = FALSE)
  
  expect_identical(
    names(res),
    c("source", "target_var", "key_id", "q", "class_n", "t_closeness")
  )
  
  conf_res <- res |>
    dplyr::filter(source == "confidential") |>
    dplyr::arrange(q)
  
  # class A: 3 records; class B: 2 records
  expect_equal(conf_res$class_n, c(3, 2))
  
  # overall confidential distribution: X = 4/5, Y = 1/5
  # class A: X = 1, Y = 0 -> max(|1 - 0.8|, |0 - 0.2|) = 0.2
  # class B: X = 0.5, Y = 0.5 -> max(|0.5 - 0.8|, |0.5 - 0.2|) = 0.3
  expect_equal(conf_res$t_closeness, c(0.2, 0.3))
  
})

test_that("scan_t_closeness summarize = TRUE reports the worst-case (maximum) class", {
  
  res <- scan_t_closeness(toy_scan)
  
  expect_identical(
    names(res),
    c("source", "target_var", "key_id", "q", "class_n", "t_closeness")
  )
  
  # one row per source/target_var
  expect_equal(nrow(res), 2)
  
  conf_res <- res |>
    dplyr::filter(source == "confidential")
  
  # class A (n=3) has t_closeness 0.2, class B (n=2) has t_closeness 0.3 -> maximum is class B
  expect_equal(conf_res$q, factor("B", levels = c("A", "B")))
  expect_equal(conf_res$class_n, 2)
  expect_equal(conf_res$t_closeness, 0.3)
  
  synth_res <- res |>
    dplyr::filter(source == "synthetic")
  
  # overall synthetic distribution: X = 3/5, Y = 2/5
  # class A (n=2): X=0.5,Y=0.5 -> max(|0.5-0.6|,|0.5-0.4|) = 0.1
  # class B (n=3): X=2/3,Y=1/3 -> max(|2/3-0.6|,|1/3-0.4|) = 0.0667 -> maximum is class A
  expect_equal(synth_res$q, factor("A", levels = c("A", "B")))
  expect_equal(synth_res$class_n, 2)
  expect_equal(synth_res$t_closeness, 0.1, tolerance = 1e-8)
  
})

test_that("scan_t_closeness supports l1 and l2 metrics", {
  
  conf_res_l1 <- scan_t_closeness(toy_scan, metric = "l1", summarize = FALSE) |>
    dplyr::filter(source == "confidential") |>
    dplyr::arrange(q)
  
  # class A: |1 - 0.8| + |0 - 0.2| = 0.4; class B: |0.5-0.8| + |0.5-0.2| = 0.6
  expect_equal(conf_res_l1$t_closeness, c(0.4, 0.6))
  
  conf_res_l2 <- scan_t_closeness(toy_scan, metric = "l2", summarize = FALSE) |>
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
  
  res <- scan_t_closeness(scan, summarize = FALSE)
  
  expect_equal(res$t_closeness[res$source == "confidential"], c(0, 0))
  
})

test_that("scan_t_closeness omits holdout when eval_data$holdout_data is not supplied", {
  
  res <- scan_t_closeness(toy_scan, summarize = FALSE)
  
  expect_false("holdout" %in% res$source)
  
})

test_that("scan_t_closeness includes holdout when eval_data$holdout_data is supplied", {
  
  res <- scan_t_closeness(toy_scan_holdout, summarize = FALSE) |>
    dplyr::filter(source == "holdout") |>
    dplyr::arrange(q)
  
  # class A: 1 record; class B: 4 records
  expect_equal(res$class_n, c(1, 4))
  
  # overall holdout distribution: X = 3/5, Y = 2/5
  # class A: X = 1, Y = 0 -> max(|1 - 0.6|, |0 - 0.4|) = 0.4
  # class B: X = 0.5, Y = 0.5 -> max(|0.5 - 0.6|, |0.5 - 0.4|) = 0.1
  expect_equal(res$t_closeness, c(0.4, 0.1))
  
})
