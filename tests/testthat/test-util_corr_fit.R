# confidential data
df <- data.frame(a = c(1, 2, 3),
                 b = c(1, 2, 3),
                 c = c(1, 2, 3),
                 RECID = c("a", "b", "c"))

# difference matrix for tests
diff_matrix <- matrix(
  c(NA, NA, NA,
    -2, NA, NA,
    0, -2, NA),
  ncol = 3,
  byrow = TRUE
) 

rownames(diff_matrix) <- c("a", "c", "b")
colnames(diff_matrix) <- c("a", "c", "b")

# test with postsynth
test_that("util_corr_fit is correct with postsynth ", {
  
  syn <- list(synthetic_data = data.frame(a = c(1, 2, 3),
                                          c = c(3, 2, 1),
                                          b = c(1, 2, 3),
                                          RECID = c("a", "b", "c"))) %>%
    structure(class = "postsynth")
  
  ed <- eval_data(conf_data = df, synth_data = syn)
  
  corr <- util_corr_fit(ed)
  
  expect_equal(corr$correlation_difference, diff_matrix)
  expect_equal(corr$correlation_fit, sqrt(sum(c(0, -2, -2) ^ 2)) / 3)
  expect_equal(corr$correlation_difference_mae, mean(abs(c(0, -2, -2))))
  expect_equal(corr$correlation_difference_rmse, sqrt(mean(c(0, -2, -2) ^ 2)))
  
})

# test with data
test_that("util_corr_fit is correct with postsynth ", {
  
  syn <- data.frame(a = c(1, 2, 3),
                    c = c(3, 2, 1),
                    b = c(1, 2, 3),
                    RECID = c("a", "b", "c"))
  
  ed <- eval_data(conf_data = df, synth_data = syn)
  
  corr <- util_corr_fit(ed)

  expect_equal(corr$correlation_difference, diff_matrix)
  expect_equal(corr$correlation_fit, sqrt(sum(c(0, -2, -2) ^ 2)) / 3)
  expect_equal(corr$correlation_difference_mae, mean(abs(c(0, -2, -2))))
  expect_equal(corr$correlation_difference_rmse, sqrt(mean(c(0, -2, -2) ^ 2)))
})

test_that("util_corr_fit works with NA ", {
  
  syn <- list(
    synthetic_data = acs_conf
  ) %>%
    structure(class = "postsynth")
  
  ed <- eval_data(synth_data = syn, conf_data = acs_conf)
  
  corr <- util_corr_fit(eval_data = ed, use = "pairwise.complete.obs")
  
  expect_equal(max(corr$correlation_difference, na.rm = TRUE), 0)
  expect_equal(corr$correlation_fit, 0)
  expect_equal(corr$correlation_difference_mae, 0)
  expect_equal(corr$correlation_difference_rmse, 0)
})
