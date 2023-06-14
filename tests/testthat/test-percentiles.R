# test df (8 /8 combinations)
df <- data.frame(
  a = c(1000, 1000, 1000),
  b = c(-1200, -800, 1000),
  c = c("1", "1", "2"),
  d = c(0, 0, 10), 
  weight = c(300, 100, 100)
)

# test synth (4 of 8 combinations)
syn <- list(
  synthetic_data = data.frame(
    a = c(1400, 0, 1000),
    b = c(1000, 1000, 1000),
    c = c("1", "1", "2"),
    d = c(20, 10, 0), 
    weight = c(300, 100, 100)
  ),
  jth_synthesis_time = data.frame(
    variable = factor(c("a", "b", "d", "weight"))
  )
) %>%
  structure(class = "postsynth")

test_that("unweighted percentiles make sense ", {
  
  test1 <- percentiles(postsynth = syn, data = df, probs = 0.5)
  
  # does the dimension make sense?
  expect_equal(
    dim(test1),
    c(4, 6)
  )
  
  # do the original values make sense
  expect_equal(
    test1$original,
    c(1000, -800, 0, 100)
  )
  
  # do the synthetic values make sense
  expect_equal(
    test1$synthetic,
    c(1000, 1000, 10, 100)
  )

})

test_that("weighted percentiles make sense ", {
  
  test2 <- percentiles(
    postsynth = syn, 
    data = df, 
    probs = 0.5,
    weight_var = weight
  )
  
  # does the dimension make sense?
  expect_equal(
    dim(test2),
    c(4, 6)
  )
  
  # do the original values make sense
  expect_equal(
    test2$original,
    c(1000, -1200, 0, 300)
  )
  
  # do the synthetic values make sense
  expect_equal(
    test2$synthetic,
    c(1400, 1000, 20, 300)
  )
  
})

test_that("percentiles can handle multiple percentile ", {

  test3 <- percentiles(postsynth = syn, probs = c(0.01, 0.99), data = df, weight_var = weight)
  
  # does the dimension make sense?
  expect_equal(
    dim(test3),
    c(8, 6)
  )
  
  test4 <- percentiles(
    postsynth = syn, 
    data = df, 
    probs = c(0.1, 0.5, 0.9),
    weight_var = weight
  )
  
  # are the correct p labels returned
  expect_equal(
    unique(test4$p),
    c(0.1, 0.5, 0.9)
  )
  
})
