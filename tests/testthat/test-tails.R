# test df (8 /8 combinations)
df <- data.frame(
  a = c(1000, 1000, 1000),
  b = c(-1200, -800, 1000),
  c = c("1", "1", "2"),
  d = c(0, 0, 10), 
  weight = c(100, 100, 100)
)

# test synth (4 of 8 combinations)
syn <- list(
  synthetic_data = data.frame(
    a = c(1400, 0, 1000),
    b = c(1000, 1000, 1000),
    c = c("1", "1", "2"),
    d = c(20, 10, 0), 
    weight = c(100, 100, 100)
  ),
  jth_synthesis_time = data.frame(
    variable = factor(c("a", "b", "d", "weight"))
  )
) %>%
  structure(class = "postsynth")

test_that("unweighted tails make sense ", {
  
  test1 <- tails(postsynth = syn, data = df, n = 2)
  
  # does the dimension make sense?
  expect_equal(
    dim(test1),
    c(16, 7)
  )
  
  # do the ranks make sense
  expect_equal(
    test1$.rank,
    c(1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2)
  )
  
  # are the weighted values equal to the unweighted values (weight = 1)
  expect_equal(
    test1$.value,
    test1$.weighted_value
  )
  
})


test_that("weighted tails make sense ", {
  
  test2 <- tails(syn, df, weight_var = weight, n = 2)
  
  expect_equal(
    test2$.value * 100,
    test2$.weighted_value
  )
  
  test2_subset <- dplyr::filter(test2, variable == "d")
  
  # check the weighted value column
  expect_equal(
    test2_subset$.weighted_value,
    c(1000, 0, 2000, 1000)
  )
  
  # check the weighted proportion column
  expect_equal(
    test2_subset$.weighted_prop,
    c(1, 0, 2 / 3, 1 / 3)
  )
  
  # check the cumulative proportion column
  expect_equal(
    test2_subset$.cumulative_prop,
    c(1, 1, 2 / 3, 1)
  )

})