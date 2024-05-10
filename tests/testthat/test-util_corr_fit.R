library(tidyverse)

# confidential data
df <- data.frame(a = c(1, 2, 3, 4, 5),
                 b = c(1, 2, 3, 4, 5),
                 c = c(1, 2, 3, 4, 5),
                 d = c("red", "red", "yellow", "yellow", "yellow"),
                 e = c("blue", "blue", "green", "green", "green"))

syn <- list(synthetic_data = data.frame(a = c(1, 2, 3, 4, 5),
                                        b = c(5, 4, 3, 2, 1),
                                        c = c(1, 2, 3, 4, 5),
                                        d = c("red", "red", "yellow", "yellow", "yellow"),
                                        e = c("blue", "blue", "green", "green", "green"))) %>%
  structure(class = "postsynth")

# test with postsynth
test_that("util_corr_fit is correct with postsynth ", {
  
  corr <- util_corr_fit(postsynth = syn, data = df)
  corr_data <- corr$correlation_data 
  metrics <- corr$metrics
  
  # check dimensions
  expect_equal(ncol(corr_data), 6)
  expect_equal(nrow(corr_data), 3)
  expect_equal(ncol(metrics), 4)
  expect_equal(nrow(metrics), 1)
  
  expect_equal(corr_data$difference, c(2, 0, 2))
  expect_equal(metrics$correlation_fit, sqrt(sum(c(0, -2, -2) ^ 2)) / 3)
  expect_equal(metrics$correlation_difference_mae, mean(abs(c(0, -2, -2))))
  expect_equal(metrics$correlation_difference_rmse, sqrt(mean(c(0, -2, -2) ^ 2)))
  # throws an error, may be due to how the n column was added to the data
  expect_equal(metrics$n, 5)
  
})

# test with data
test_that("util_corr_fit is correct with postsynth ", {
  
  syn_data <- data.frame(a = c(1, 2, 3, 4, 5),
                         b = c(5, 4, 3, 2, 1),
                         c = c(1, 2, 3, 4, 5),
                         d = c("red", "red", "yellow", "yellow", "yellow"),
                         e = c("blue", "blue", "green", "green", "green"))
  
  corr <- util_corr_fit(postsynth = syn_data, data = df)
  corr_data <- corr$correlation_data 
  metrics <- corr$metrics
  
  # check dimensions
  expect_equal(ncol(corr_data), 6)
  expect_equal(nrow(corr_data), 3)
  expect_equal(ncol(metrics), 4)
  expect_equal(nrow(metrics), 1)
  
  expect_equal(corr_data$difference, c(2, 0, 2))
  expect_equal(metrics$correlation_fit, sqrt(sum(c(0, -2, -2) ^ 2)) / 3)
  expect_equal(metrics$correlation_difference_mae, mean(abs(c(0, -2, -2))))
  expect_equal(metrics$correlation_difference_rmse, sqrt(mean(c(0, -2, -2) ^ 2)))
  expect_equal(metrics$n, 5)
})



test_that("util_corr_fit is correct when groupped by one variable ", {
  
  df <- data.frame(a = c(1, 2, 3, 4, 5),
                   b = c(1, 2, 3, 4, 5),
                   c = c(1, 2, 3, 4, 5),
                   d = c("red", "red", "yellow", "yellow", "yellow"),
                   e = c("blue", "blue", "green", "green", "green"))
  
  syn <- list(synthetic_data = data.frame(a = c(1, 2, 3, 4, 5),
                                          b = c(5, 4, 3, 2, 1),
                                          c = c(1, 2, 3, 4, 5),
                                          d = c("red", "red", "yellow", "yellow", "yellow"),
                                          e = c("blue", "blue", "green", "green", "green"))) %>%
    structure(class = "postsynth")
  
  corr <- util_corr_fit(postsynth = syn, data = df, group_by = d)
  corr_data <- corr$correlation_data
  metrics <- corr$metrics
  
  # check dimensions
  expect_equal(ncol(corr_data), 7)
  expect_equal(nrow(corr_data), 6)
  expect_equal(ncol(metrics), 5)
  expect_equal(nrow(metrics), 2)
  
  expect_equal(corr_data$difference, c(2, 0, 2, 2, 0, 2))
  expect_equal(metrics$correlation_fit, rep(sqrt(sum(c(0, -2, -2) ^ 2)) / 3, 2))
  expect_equal(metrics$correlation_difference_mae, rep(mean(abs(c(0, -2, -2))), 2))
  expect_equal(metrics$correlation_difference_rmse, rep(sqrt(mean(c(0, -2, -2) ^ 2)), 2))
  expect_equal(metrics$n, c(2, 3))
  
})


test_that("util_corr_fit is correct when groupped by more than one variable ", {
  
  corr <- util_corr_fit(postsynth = syn, data = df, group_by = c(d, e))
  corr_data <- corr$correlation_data 
  metrics <- corr$metrics
  
  # check dimensions
  expect_equal(ncol(corr_data), 8)
  expect_equal(nrow(corr_data), 6)
  expect_equal(ncol(metrics), 6)
  expect_equal(nrow(metrics), 2)
  
  expect_equal(corr_data$difference, c(2, 0, 2, 2, 0, 2))
  expect_equal(metrics$correlation_fit, rep(sqrt(sum(c(0, -2, -2) ^ 2)) / 3, 2))
  expect_equal(metrics$correlation_difference_mae, rep(mean(abs(c(0, -2, -2))), 2))
  expect_equal(metrics$correlation_difference_rmse, rep(sqrt(mean(c(0, -2, -2) ^ 2)), 2))
  expect_equal(metrics$n, c(2, 3))
  
})


test_that("util_corr_fit returns NAs when only one observation is in a grouping ", {
  
  df <- data.frame(a = c(1, 2, 3, 4, 5),
                   b = c(1, 2, 3, 4, 5),
                   c = c(1, 2, 3, 4, 5),
                   d = c("red", "red", "yellow", "yellow", "red"),
                   e = c("blue", "blue", "green", "green", "green"),
                   RECID = c("a", "b", "c", "d", "e"))
  
  syn <- data.frame(a = c(1, 2, 3, 4, 5),
                    b = c(5, 4, 3, 2, 1),
                    c = c(1, 2, 3, 4, 5),
                    d = c("red", "red", "yellow", "yellow", "red"),
                    e = c("blue", "blue", "green", "green", "green"),
                    RECID = c("a", "b", "c", "d", "e")) 
  
  corr <- util_corr_fit(postsynth = syn, data = df, group_by = c(d, e))
  corr_data <- corr$correlation_data
  metrics <- corr$metrics
  
  expect_equal(ncol(corr_data), 8)
  expect_equal(nrow(corr_data), 9)
  expect_equal(ncol(metrics), 6)
  expect_equal(nrow(metrics), 3)
  
  expect_equal(corr_data$difference, c(2, 0, 2, NA, NA, NA, 2, 0, 2))
  expect_equal(metrics$correlation_fit, c(sqrt(sum(c(0, -2, -2) ^ 2)) / 3, NaN, sqrt(sum(c(0, -2, -2) ^ 2)) / 3))
  expect_equal(metrics$correlation_difference_mae, c(mean(abs(c(0, -2, -2))), NA, mean(abs(c(0, -2, -2)))))
  expect_equal(metrics$correlation_difference_rmse, c(sqrt(mean(c(0, -2, -2) ^ 2)), NA, sqrt(mean(c(0, -2, -2) ^ 2))))
  expect_equal(metrics$n, c(2, 1, 2))
  
})

# NOTE: These tests will fail

test_that("util_corr_fit returns NAs when a grouping in the actual data is not in the synthetic data", {
  
  df <- data.frame(a = c(1, 2, 3, 4, 5),
                   b = c(1, 2, 3, 4, 5),
                   c = c(1, 2, 3, 4, 5),
                   d = c("red", "red", "yellow", "yellow", "red"),
                   e = c("blue", "blue", "green", "green", "green"),
                   RECID = c("a", "b", "c", "d", "e"))
  
  syn <- data.frame(a = c(1, 2, 3, 4, 5),
                    b = c(5, 4, 3, 2, 1),
                    c = c(1, 2, 3, 4, 5),
                    d = c("red", "red", "yellow", "yellow", "yellow"),
                    e = c("blue", "blue", "green", "green", "green"),
                    RECID = c("a", "b", "c", "d", "e")) 
  
  corr <- util_corr_fit(postsynth = syn, data = df, group_by = c(d, e))
  corr_data <- corr$correlation_data 
  
  expect_equal(corr_data$difference, c(2, 0, 2, 2, 0, 2, NA, NA, NA))
  expect_equal(corr$correlation_fit, c(rep(sqrt(sum(c(0, -2, -2) ^ 2)) / 3, 2), NaN))
  expect_equal(corr$correlation_difference_mae, c(rep(mean(abs(c(0, -2, -2))), 2), NA))
  expect_equal(corr$correlation_difference_rmse, c(rep(sqrt(mean(c(0, -2, -2) ^ 2)), 2), NA))
  
})


test_that("util_corr_fit returns NAs when a grouping in the synthetic data is not in the actual data", {
  
  df <- data.frame(a = c(1, 2, 3, 4, 5),
                   b = c(1, 2, 3, 4, 5),
                   c = c(1, 2, 3, 4, 5),
                   d = c("red", "red", "yellow", "yellow", "yellow"),
                   e = c("blue", "blue", "green", "green", "green"),
                   RECID = c("a", "b", "c", "d", "e"))
  
  syn <- data.frame(a = c(1, 2, 3, 4, 5),
                    b = c(5, 4, 3, 2, 1),
                    c = c(1, 2, 3, 4, 5),
                    d = c("red", "red", "yellow", "yellow", "red"),
                    e = c("blue", "blue", "green", "green", "green"),
                    RECID = c("a", "b", "c", "d", "e")) 
  
  corr <- util_corr_fit(postsynth = syn, data = df, group_by = c(d, e))
  corr_data <- corr$correlation_data 
  
  expect_equal(corr_data$difference, c(2, 0, 2, 2, 0, 2, NA, NA, NA))
  expect_equal(corr$correlation_fit, c(rep(sqrt(sum(c(0, -2, -2) ^ 2)) / 3, 2), NaN))
  expect_equal(corr$correlation_difference_mae, c(rep(mean(abs(c(0, -2, -2))), 2), NA))
  expect_equal(corr$correlation_difference_rmse, c(rep(sqrt(mean(c(0, -2, -2) ^ 2)), 2), NA))
  
})





