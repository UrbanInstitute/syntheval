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
  
  corr <- util_corr_fit(postsynth = syn, data = df)
  
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
  
  corr <- util_corr_fit(postsynth = syn, data = df)

  expect_equal(corr$correlation_difference, diff_matrix)
  expect_equal(corr$correlation_fit, sqrt(sum(c(0, -2, -2) ^ 2)) / 3)
  expect_equal(corr$correlation_difference_mae, mean(abs(c(0, -2, -2))))
  expect_equal(corr$correlation_difference_rmse, sqrt(mean(c(0, -2, -2) ^ 2)))
})
