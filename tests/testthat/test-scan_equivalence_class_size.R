
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

test_that("scan_equivalence_class_size input errors", {
  
  expect_error(scan_equivalence_class_size("not an attribute_scan object"))
  
})

test_that("scan_equivalence_class_size basic functionality", {
  
  res <- scan_equivalence_class_size(toy_scan)
  
  expect_identical(
    names(res),
    c("source", "n_classes", "min_size", "mean_size", "median_size", "max_size")
  )
  
  # confidential: class A n=3, class B n=2
  conf_res <- res |> dplyr::filter(source == "confidential")
  
  expect_equal(conf_res$n_classes, 2)
  expect_equal(conf_res$min_size, 2)
  expect_equal(conf_res$mean_size, 2.5)
  expect_equal(conf_res$median_size, 2.5)
  expect_equal(conf_res$max_size, 3)
  
  # synthetic: class A n=2, class B n=3
  synth_res <- res |> dplyr::filter(source == "synthetic")
  
  expect_equal(synth_res$n_classes, 2)
  expect_equal(synth_res$min_size, 2)
  expect_equal(synth_res$mean_size, 2.5)
  expect_equal(synth_res$median_size, 2.5)
  expect_equal(synth_res$max_size, 3)
  
})

test_that("scan_equivalence_class_size ignores unobserved classes", {
  
  na_conf <- data.frame(
    q = factor(c(NA, "A", "A", "B", "B"), levels = c("A", "B")),
    t = factor(c("X", "X", "X", "Y", "X"), levels = c("X", "Y"))
  )
  
  scan <- attribute_scan(
    eval_data(conf_data = na_conf, synth_data = na_conf),
    qid_keys = "q",
    target_keys = "t"
  )
  
  res <- scan_equivalence_class_size(scan)
  
  # 3 observed classes: q = A (n=2), q = B (n=2), q = NA (n=1)
  expect_equal(res$n_classes[res$source == "confidential"], 3)
  expect_equal(res$min_size[res$source == "confidential"], 1)
  
})
