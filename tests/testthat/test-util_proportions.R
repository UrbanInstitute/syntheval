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

# with group_by specified 
test_that("testing if proportions are correct w/ group_by -- postsynth", {
  
  summary_stats <-
    util_proportions(
      postsynth = syn, 
      data = df,
      group_by = c
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

test_that("testing if proportions are correct w/ group_by -- df", {
  
  summary_stats <-
    util_proportions(
      postsynth = df, 
      data = df,
      group_by = c
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


# with group_by and weight_var specified 
test_that("testing if proportions w/ weight_var and group_by are correct
          -- postsynth", {
  
  summary_stats <-
    util_proportions(
      postsynth = syn, 
      data = df,
      weight_var = weight,
      group_by = c
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


test_that("testing if proportions w/ weight_var and group_by are correct 
          -- df", {
  
  summary_stats <-
    util_proportions(
      postsynth = df, 
      data = df,
      weight_var = weight,
      group_by = c
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

test_that("test util_proportions() with multiple grouping variables", {
  
  # original data
  df2 <- data.frame(
    var1 = c(1, 1, 2),
    var2 = c("orange", "orange", "green"),
    var3 = as.factor(c("1", "1", "2")),
    var4 = c("a", "a", "b"), 
    weight = c(100, 100, 200)
  )
  
  # postsynth data object
  syn2 <- list(
    synthetic_data = data.frame(
      var1 = c(1, 1, 2),
      var2 = c("orange", "orange", "green"),
      var3 = as.factor(c("1", "1", "2")),
      var4 = c("a", "a", "b"),      
      weight = c(50, 150, 200)
    )
  ) %>%
    structure(class = "postsynth")
  
  
  summary_stats <-
    util_proportions(
      postsynth = syn2, 
      data = df2,
      weight_var = weight,
      group_by = c(var2, var3)
    ) 
  
  expect_equal(
    summary_stats$original,
    c(1, 1)
  )
  expect_equal(
    summary_stats$synthetic,
    c(1, 1)
  )
  expect_equal(
    summary_stats$difference,
    c(0, 0)
  )
  
})



