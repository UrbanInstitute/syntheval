# original data
df <- data.frame(
  a = c(1, 2, 1),
  b = c("orange", "yellow", "green"),
  c = as.factor(c("1", "1", "2")),
  weight = c(100, 100, 200)
)

# postsynth data object
syn <- list(
  synthetic_data = data.frame(
    a = c(2, 2, 2),
    b = c("orange", "yellow", "green"),
    c = as.factor(c("1", "2", "2")),
    weight = c(150, 150, 100)
  )
) %>%
  structure(class = "postsynth")

# testing variable selection
test_that("testing if proportions only uses fct and chr variables", {
  
  summary_stats <-
    util_proportions(
      postsynth = syn, 
      data = df
    ) 
  
  expect_equal(
    unique(summary_stats$variable),
    c("b", "c")
  )
})

# testing proportions
test_that("testing if proportions are correct -- postsynth", {
  
  summary_stats <-
    util_proportions(
      postsynth = syn, 
      data = df
    ) 
  
  expect_equal(
    round(summary_stats$original, 3),
    c(0.333, 0.333, 0.333, 0.667, 0.333)
  )
  expect_equal(
    round(summary_stats$synthetic, 3),
    c(0.333, 0.333, 0.333, 0.333, 0.667)
  )
  
})


test_that("testing if proportions are correct -- df", {
  
  summary_stats <-
    util_proportions(
      postsynth = df, 
      data = df
    ) 
  
  expect_equal(
    round(summary_stats$original, 3),
    c(0.333, 0.333, 0.333, 0.667, 0.333)
  )
  expect_equal(
    round(summary_stats$synthetic, 3),
    c(0.333, 0.333, 0.333, 0.667, 0.333)
  )
  
})

# with group_var specified 
test_that("testing if proportions are correct w/ group_var -- postsynth", {
  
  summary_stats <-
    util_proportions(
      postsynth = syn, 
      data = df,
      group_var = c
    ) 
  
  expect_equal(
    summary_stats$original,
    c(0.5, 0.5, 1, 0)
  )
  expect_equal(
    summary_stats$synthetic,
    c(1, 0, 0.5, 0.5)
  )
  
})

test_that("testing if proportions are correct w/ group_var -- df", {
  
  summary_stats <-
    util_proportions(
      postsynth = df, 
      data = df,
      group_var = c
    ) 
  
  expect_equal(
    summary_stats$original,
    c(0.5, 0.5, 1)
  )
  expect_equal(
    summary_stats$synthetic,
    c(0.5, 0.5, 1)
  )
  
})

# with weight_var specified  
test_that("testing if proportions w/ weight_var are correct  -- postsynth", {
  
  summary_stats <-
    util_proportions(
      postsynth = syn, 
      data = df,
      weight_var = weight
    ) 
  
  expect_equal(
    summary_stats$original,
    c(0.5, 0.25, 0.25, 0.5, 0.5)
  )
  expect_equal(
    summary_stats$synthetic,
    c(0.25, 0.375, 0.375, 0.375, 0.625)
  )
  
})


test_that("testing if proportions w/ weight_var are correct -- df", {
  
  summary_stats <-
    util_proportions(
      postsynth = df, 
      data = df,
      weight_var = weight
    ) 
  
  expect_equal(
    summary_stats$original,
    c(0.5, 0.25, 0.25, 0.5, 0.5)
  )
  expect_equal(
    summary_stats$synthetic,
    c(0.5, 0.25, 0.25, 0.5, 0.5)
  )
  
})


# with group_var and weight_var specified 
test_that("testing if proportions w/ weight_var and group_var are correct
          -- postsynth", {
  
  summary_stats <-
    util_proportions(
      postsynth = syn, 
      data = df,
      weight_var = weight,
      group_var = c
    ) 
  
  expect_equal(
    summary_stats$original,
    c(0.5, 0.5, 1, 0)
  )
  expect_equal(
    summary_stats$synthetic,
    c(1, 0, 0.4, 0.6)
  )
  
})


test_that("testing if proportions w/ weight_var and group_var are correct 
          -- df", {
  
  summary_stats <-
    util_proportions(
      postsynth = df, 
      data = df,
      weight_var = weight,
      group_var = c
    ) 
  
  expect_equal(
    summary_stats$original,
    c(0.5, 0.5, 1)
  )
  expect_equal(
    summary_stats$synthetic,
    c(0.5, 0.5, 1)
  )
  
})
