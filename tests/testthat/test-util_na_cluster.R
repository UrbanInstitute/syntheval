conf <- data.frame(
  a = c(NA, 2, NA, 4),
  b = c(NA, 2, NA, 4),
  c = c(1, NA, 3, 4)
)

synth_same <- data.frame(
  a = c(NA, 2, NA, 4),
  b = c(NA, 2, NA, 4),
  c = c(1, NA, 3, 4)
)

expected_matrix <- function(data) {
  
  phi <- stats::cor(is.na(data) * 1)
  phi[upper.tri(phi, diag = TRUE)] <- NA
  phi
  
}

test_that("util_na_cluster is 0 when conf and synth are identical", {
  
  ed <- eval_data(conf_data = conf, synth_data = synth_same)
  
  result <- util_na_cluster(ed)
  
  expect_equal(result$na_cluster_original, expected_matrix(conf))
  expect_equal(result$na_cluster_difference_mae, 0)
  expect_equal(result$na_cluster_difference_rmse, 0)
  
})

test_that("util_na_cluster detects columns that are always missing together", {
  
  ed <- eval_data(conf_data = conf, synth_data = synth_same)
  
  result <- util_na_cluster(ed)
  
  # the missingness is perfectly correlated in the two columns
  expect_equal(result$na_cluster_original["b", "a"], 1)
  
})

synth_diff <- data.frame(
  a = c(NA, 2, 3, 4),
  b = c(1, 2, NA, 4),
  c = c(1, NA, 3, 4)
)

test_that("util_na_cluster detects differences between conf and synth", {
  
  ed <- eval_data(conf_data = conf, synth_data = synth_diff)
  
  result <- util_na_cluster(ed)
  
  expect_equal(result$na_cluster_original, expected_matrix(conf))
  expect_equal(result$na_cluster_synthetic, expected_matrix(synth_diff))
  expect_equal(
    result$na_cluster_difference,
    expected_matrix(synth_diff) - expected_matrix(conf)
  )
  
})

conf_sentinel <- data.frame(
  a = c(-99, 2, -99, 4),
  b = c(-99, 2, -99, 4),
  c = c(1, -99, 3, 4)
)

test_that("util_na_cluster respects a custom na_values sentinel", {
  
  ed <- eval_data(conf_data = conf_sentinel, synth_data = conf_sentinel)
  
  result <- util_na_cluster(ed, na_values = -99)
  
  # the missingness is perfectly correlated in the two columns
  expect_equal(result$na_cluster_original["b", "a"], 1)
  expect_equal(result$na_cluster_difference_mae, 0)
  
})

holdout <- data.frame(
  a = c(NA, 2, NA, 4),
  b = c(1, 2, 3, 4),
  c = c(1, NA, 3, 4)
)

test_that("util_na_cluster includes a na_cluster_holdout matrix when supplied", {
  
  ed <- eval_data(conf_data = conf, synth_data = synth_same, holdout_data = holdout)
  
  result <- util_na_cluster(ed)
  
  expect_equal(result$na_cluster_holdout, expected_matrix(holdout))
  
})
