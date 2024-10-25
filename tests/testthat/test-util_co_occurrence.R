# confidential data
df <- data.frame(a = c(1, 0, 1, 2),
                 b = c(1, 2, 0, 2),
                 c = c("a", "a", "b", "b"))

syn <- list(synthetic_data = data.frame(a = c(1, 0, 0, 0),
                                        c = c("c", "c", "c", "c"),
                                        b = c(1, 0, 0, 0))) %>%
  structure(class = "postsynth")

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

test_that("util_co_occurrence() works with NA ", {
  
  syn <- list(
    synthetic_data = acs_conf
  ) %>%
    structure(class = "postsynth")
  
  co_occurrence <- util_co_occurrence(
    postsynth = syn, 
    data = acs_conf,
    na.rm = TRUE
  )

  expect_equal(max(co_occurrence$co_occurrence_difference, na.rm = TRUE), 0)
  expect_equal(co_occurrence$co_occurrence_difference_mae, 0)
  expect_equal(co_occurrence$co_occurrence_difference_rmse, 0)
  
})
