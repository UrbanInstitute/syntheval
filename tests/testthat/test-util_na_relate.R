conf_mixed <- data.frame(
  x = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
  group = rep(c("a", "b"), each = 5),
  y = c(NA, NA, NA, NA, NA, 20, 21, 22, 23, 24)
)

synth_mixed_same <- conf_mixed

test_that("util_na_relate detects clear numeric (t-test) and categorical (fisher) associations", {

  ed <- eval_data(conf_data = conf_mixed, synth_data = synth_mixed_same)

  result <- util_na_relate(ed)

  row_numeric <- dplyr::filter(result$na_relate, na_var == "y", related_var == "x")

  expect_equal(row_numeric$method, "t_test")
  expect_true(row_numeric$p_value_adj_confidential < 0.05)
  expect_equal(row_numeric$effect_size_confidential, row_numeric$effect_size_synthetic)

  row_categorical <- dplyr::filter(result$na_relate, na_var == "y", related_var == "group")

  expect_equal(row_categorical$method, "fisher")
  expect_true(row_categorical$p_value_adj_confidential < 0.05)
  expect_equal(row_categorical$effect_size_confidential, row_categorical$effect_size_synthetic)

})

conf_sentinel <- data.frame(
  x = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
  y = c(-99, -99, -99, -99, -99, 20, 21, 22, 23, 24)
)

test_that("util_na_relate respects a custom na_values sentinel", {

  ed <- eval_data(conf_data = conf_sentinel, synth_data = conf_sentinel)

  result <- util_na_relate(ed, na_values = -99)

  row <- dplyr::filter(result$na_relate, na_var == "y", related_var == "x")

  expect_equal(row$method, "t_test")
  expect_true(row$p_value_adj_confidential < 0.05)

})

test_that("util_na_relate_summary classification matches a hand-computed example", {

  ed <- eval_data(conf_data = conf_mixed, synth_data = synth_mixed_same)

  result <- util_na_relate(ed)

  summary_row <- dplyr::filter(result$na_relate_summary, na_var == "y")

  expect_equal(summary_row$preserved, 2)
  expect_equal(summary_row$lost, 0)
  expect_equal(summary_row$spurious, 0)
  expect_equal(summary_row$concordance_rate, 1)

  overall_row <- dplyr::filter(result$na_relate_summary, na_var == "overall")

  expect_equal(overall_row$n_pairs, nrow(result$na_relate))

})

test_that("util_na_relate returns NA for effect sizes it cannot estimate", {

  ed <- eval_data(conf_data = conf_mixed, synth_data = synth_mixed_same)

  result <- util_na_relate(ed)

  # group is perfectly separated by y's missingness, so the odds ratio is infinite
  row <- dplyr::filter(result$na_relate, related_var == "group")

  expect_true(is.na(row$effect_size_confidential))
  expect_true(row$p_value_adj_confidential < 0.05)

  # only the t_test pair contributes to the mean absolute error
  summary_row <- dplyr::filter(result$na_relate_summary, na_var == "y")

  expect_equal(summary_row$n_effect_size, 1)

})

test_that("util_na_relate alpha controls the significance classification", {

  ed <- eval_data(conf_data = conf_mixed, synth_data = synth_mixed_same)

  result <- util_na_relate(ed, alpha = 0.0001)

  summary_row <- dplyr::filter(result$na_relate_summary, na_var == "y")

  expect_equal(summary_row$preserved, 0)
  expect_equal(summary_row$consistent_null, 2)
  expect_true(is.na(summary_row$concordance_rate))

})

conf_extra <- dplyr::mutate(conf_mixed, only_in_conf = 1:10)
synth_extra <- dplyr::mutate(synth_mixed_same, synth_id = 1:10)

test_that("util_na_relate only compares variables common to conf and synth", {

  ed <- eval_data(conf_data = conf_extra, synth_data = synth_extra)

  result <- util_na_relate(ed)

  expect_setequal(result$na_relate$related_var, c("x", "group"))

})

test_that("util_na_relate keeps conf/synth pairs the holdout cannot support", {

  ed <- eval_data(
    conf_data = conf_mixed,
    synth_data = synth_mixed_same,
    holdout_data = conf_mixed[, c("x", "y")]
  )

  result <- util_na_relate(ed)

  # group is absent from the holdout, but conf vs. synth still compares it
  expect_setequal(result$na_relate$related_var, c("x", "group"))

  row_group <- dplyr::filter(result$na_relate, related_var == "group")

  expect_true(is.na(row_group$effect_size_holdout))
  expect_true(is.na(row_group$p_value_adj_holdout))

  row_x <- dplyr::filter(result$na_relate, related_var == "x")

  expect_false(is.na(row_x$effect_size_holdout))

})
