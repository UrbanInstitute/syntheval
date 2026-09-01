# confidential data
df <- data.frame(a = c(1, 2, 3),
                 b = c(1, 2, 3),
                 c = c(1, 2, 3),
                 RECID = c("a", "b", "c"))

diff_table <- tibble::tibble(
  var1 = c("a", "a", "a", "c", "c", "c", "b", "b", "b"),
  var2 = c("a", "c", "b", "a", "c", "b", "a", "c", "b"),
  difference = c(0, -2, 0, -2, 0, -2, 0, -2, 0)
)

# test with postsynth, ungrouped
test_that("util_corr_fit is correct with postsynth, ungrouped", {
  
  syn <- list(synthetic_data = data.frame(a = c(1, 2, 3),
                                          c = c(3, 2, 1),
                                          b = c(1, 2, 3),
                                          RECID = c("a", "b", "c")),
              jth_synthesis_time = data.frame(
                variable = factor(c("a", "c", "b"))
              )) %>%
    structure(class = "postsynth")
  
  ed <- eval_data(conf_data = df, synth_data = syn)
  
  corr <- util_corr_fit(ed)
  
  actual_diff <- corr$correlation_difference %>%
    dplyr::select(var1, var2, difference) %>%
    dplyr::arrange(var1, var2)

  expected_diff <- diff_table %>%
    dplyr::arrange(var1, var2)

  
  expect_equal(actual_diff, expected_diff)
  expect_equal(
    corr$correlation_fit,
    sqrt(sum(expected_diff$difference ^ 2)) / nrow(expected_diff)
  )
  expect_equal(
    corr$correlation_difference_mae,
    mean(abs(expected_diff$difference))
  )
  expect_equal(
    corr$correlation_difference_rmse,
    sqrt(mean(expected_diff$difference ^ 2))
  )
  
})

# test with data
test_that("util_corr_fit is correct with postsynth, ungrouped", {
  
  syn <- data.frame(a = c(1, 2, 3),
                    c = c(3, 2, 1),
                    b = c(1, 2, 3),
                    RECID = c("a", "b", "c"))
  
  ed <- eval_data(conf_data = df, synth_data = syn)
  
  corr <- util_corr_fit(ed)

  actual_diff <- corr$correlation_difference %>%
    dplyr::select(var1, var2, difference) %>%
    dplyr::arrange(var1, var2)
  expected_diff <- diff_table %>%
    dplyr::arrange(var1, var2)

  expect_equal(actual_diff, expected_diff)
  expect_equal(
    corr$correlation_fit,
    sqrt(sum(expected_diff$difference ^ 2)) / nrow(expected_diff)
  )
  expect_equal(
    corr$correlation_difference_mae,
    mean(abs(expected_diff$difference))
  )
  expect_equal(
    corr$correlation_difference_rmse,
    sqrt(mean(expected_diff$difference ^ 2))
  )
})

test_that("util_corr_fit works with NA ", {
  
  ed <- eval_data(synth_data = acs_conf, conf_data = acs_conf)
  
  corr <- util_corr_fit(eval_data = ed, use = "pairwise.complete.obs")

  actual_diff <- corr$correlation_difference %>%
    dplyr::select(var1, var2, difference) %>%
    dplyr::arrange(var1, var2)
  
  expect_equal(max(corr$correlation_difference$difference, na.rm = TRUE), 0)
  expect_equal(corr$correlation_fit, 0)
  expect_equal(corr$correlation_difference_mae, 0)
  expect_equal(corr$correlation_difference_rmse, 0)
})

test_that("util_corr_fit works with group_by_q", {
  
  ed <- eval_data(synth_data = acs_conf, conf_data = acs_conf)
  
  corr <- util_corr_fit(eval_data = ed, use = "pairwise.complete.obs", group_by_q = "marst")

  actual_diff <- corr$correlation_difference %>%
    dplyr::select(marst, var1, var2, difference) %>%
    dplyr::arrange(marst, var1, var2)

  expect_equal(max(corr$correlation_difference$difference, na.rm = TRUE), 0)
  expect_equal(max(corr$correlation_difference_mae$correlation_difference_mae, na.rm = TRUE), 0)
  expect_equal(max(corr$correlation_difference_rmse$correlation_difference_rmse, na.rm = TRUE), 0)
  expect_equal(max(corr$correlation_fit$correlation_fit, na.rm = TRUE), 0)
})
