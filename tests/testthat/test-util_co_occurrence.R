# confidential data
df <- data.frame(a = c(1, 0, 1, 2),
                 b = c(1, 2, 0, 2),
                 c = c("a", "a", "b", "b"))

syn <- list(synthetic_data = data.frame(a = c(1, 0, 0, 0),
                                        c = c("c", "c", "c", "c"),
                                        b = c(1, 0, 0, 0))) %>%
  structure(class = "postsynth")






# # difference matrix for tests
# diff_matrix <- matrix(
#   c(NA, NA, NA,
#     -2, NA, NA,
#     0, -2, NA),
#   ncol = 3,
#   byrow = TRUE
# ) 

# rownames(diff_matrix) <- c("a", "c", "b")
# colnames(diff_matrix) <- c("a", "c", "b")

# test with postsynth
test_that("util_co_occurrence() is correct with identical data ", {

  co_occurrence <- util_co_occurrence(postsynth = df, data = df)
  
  diff_matrix <- matrix(c(NA, NA, 0, NA), byrow = TRUE, nrow = 2, ncol = 2)
  colnames(diff_matrix) <- c("a", "b")
  rownames(diff_matrix) <- c("a", "b")
  
  expect_equal(co_occurrence$co_occurrence_difference, diff_matrix)
  expect_equal(co_occurrence$co_occurrence_difference_mae, 0)
  expect_equal(co_occurrence$co_occurrence_difference_rmse, 0)
})

# test with data
test_that("util_co_occurrence() is correct with different data ", {
  
  co_occurrence <- util_co_occurrence(postsynth = syn, data = df)
  
  diff_matrix <- matrix(c(NA, NA, -0.25, NA), byrow = TRUE, nrow = 2, ncol = 2)
  colnames(diff_matrix) <- c("a", "b")
  rownames(diff_matrix) <- c("a", "b")
  
  co_occurrence <- util_co_occurrence(postsynth = syn, data = df)

  expect_equal(co_occurrence$co_occurrence_difference, diff_matrix)
  expect_equal(co_occurrence$co_occurrence_difference_mae, mean(abs(-0.25)))
  expect_equal(co_occurrence$co_occurrence_difference_rmse, sqrt(mean((-0.25) ^ 2)))
})
