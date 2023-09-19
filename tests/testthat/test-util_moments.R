# test df (8 /8 combinations)
df <- data.frame(
  a = c(1000, 1000, 1000),
  b = c(1200, 800, 1000),
  c = c("1", "1", "2"),
  d = c(0, 0, 10), 
  weight = c(100, 100, 200)
)

# test synth (4 of 8 combinations)
syn <- list(
  synthetic_data = data.frame(
    a = c(1400, 0, 1000),
    b = c(1000, 1000, 1000),
    c = c("1", "1", "2"),
    d = c(20, 10, 0), 
    weight = c(150, 150, 100)
  ),
  jth_synthesis_time = data.frame(
    variable = factor(c("a", "b", "d", "weight"))
  )
) %>%
  structure(class = "postsynth")

# full unweighted - postysynth
test_that("moments full unweighted -- postsynth ", {
  
  summary_stats <-
    moments(
      postsynth = syn, 
      data = df
    ) %>%
    dplyr::filter(variable == "a")
  
  expect_equal(
    summary_stats$original,
    c(3, 3, 1000, 0, NaN, NaN)
  )
  expect_equal(
    round(summary_stats$synthetic, 2),
    c(3, 2, 800, 721.11, -0.47, -1.5)
  )
})

test_that("moments full unweighted -- df ", {
  
  summary_stats <-
    moments(
      postsynth = df, 
      data = df
    ) %>%
    dplyr::filter(variable == "a")
  
  expect_equal(
    summary_stats$original,
    c(3, 3, 1000, 0, NaN, NaN)
  )
  expect_equal(
    round(summary_stats$synthetic, 2),
    c(3, 3, 1000, 0, NaN, NaN)
  )
})

# full weighted
test_that("moments full weighted -- postsynth", {
  
  summary_stats <-
    moments(
      postsynth = syn,
      data = df,
      weight_var = weight
    ) %>%
    dplyr::filter(variable == "a")
  
  expect_equal(
    summary_stats$original,
    c(400, 400, 1000, 0, NaN, NaN)
  )
  expect_equal(
    round(summary_stats$synthetic, 2),
    c(400, 250, 775, 765.32, -0.34, -1.69)
  )
})

test_that("moments full weighted -- df", {
  
  summary_stats <-
    moments(
      postsynth = df,
      data = df,
      weight_var = weight
    ) %>%
    dplyr::filter(variable == "a")
  
  expect_equal(
    summary_stats$original,
    c(400, 400, 1000, 0, NaN, NaN)
  )
  expect_equal(
    round(summary_stats$synthetic, 2),
    c(400, 400, 1000, 0, NaN, NaN)
  )
})

test_that("moments nonzero unweighted -- postsynth", {
  
  summary_stats <-
    moments(
      postsynth = syn,
      data = df,
      drop_zeros = TRUE
    ) %>%
    dplyr::filter(variable == "d")
  
  expect_equal(
    summary_stats$original,
    c(1, 1, 10, NaN, NaN, NaN)
  )
  expect_equal(
    round(summary_stats$synthetic, 2),
    c(2, 2, 15, 7.07, 0, -2)
  )
})

test_that("moments nonzero unweighted -- postsynth", {
  
  summary_stats <-
    moments(
      postsynth = df,
      data = df,
      drop_zeros = TRUE
    ) %>%
    dplyr::filter(variable == "d")
  
  expect_equal(
    summary_stats$original,
    c(1, 1, 10, NaN, NaN, NaN)
  )
  expect_equal(
    round(summary_stats$synthetic, 2),
    c(1, 1, 10, NaN, NaN, NaN)
  )
})

test_that("moments nonzero weighted -- postsynth", {
  
  summary_stats <-
    moments(
      postsynth = syn,
      data = df,
      weight_var = weight,
      drop_zeros = TRUE
    ) %>%
    dplyr::filter(variable == "d")
  
  expect_equal(
    summary_stats$original,
    c(200, 200, 10, NaN, NaN, NaN)
  )
  expect_equal(
    round(summary_stats$synthetic, 2),
    c(300, 300, 15, 7.07, 0, -2)
  )
})

test_that("moments nonzero weighted -- df", {
  
  summary_stats <-
    moments(
      postsynth = df,
      data = df,
      weight_var = weight,
      drop_zeros = TRUE
    ) %>%
    dplyr::filter(variable == "d")
  
  expect_equal(
    summary_stats$original,
    c(200, 200, 10, NaN, NaN, NaN)
  )
  expect_equal(
    round(summary_stats$synthetic, 2),
    c(200, 200, 10, NaN, NaN, NaN)
  )
})

test_that("moments test grouping var ", {

  summary_stats <-
    moments(
      postsynth = syn,
      data = df,
      weight_var = weight,
      group_var = c
    ) %>%
    dplyr::filter(variable == "d", statistic %in% c("count", "have", "mean"))
  
  expect_equal(
    summary_stats$original,
    c(200, 200, 0, 200, 0, 10)
  )
  expect_equal(
    round(summary_stats$synthetic, 2),
    c(300, 100, 300, 0, 15, 0)
  )


})