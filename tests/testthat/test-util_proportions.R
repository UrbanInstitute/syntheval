# original data
df <- data.frame(
  a = c(1, 2, 1),
  b = c("orange", "yellow", "green"),
  c = as.factor(c("1", "1", "2"))
)

# postsynth data object
syn <- list(
  synthetic_data = data.frame(
    a = c(2, 2, 2),
    b = c("orange", "yellow", "green"),
    c = as.factor(c("1", "2", "2"))
  ),
  jth_synthesis_time = data.frame(
    variable = factor(c("a", "b", "c"))
  )
) %>%
  structure(class = "postsynth")

# testing variable selection
test_that("proportions only uses fct and chr variables", {
  
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
test_that("proportions are correct -- postsynth", {
  
  summary_stats <-
    util_proportions(
      postsynth = syn, 
      data = df
    ) 
  
  expect_equal(
    round(summary_stats$prop, 3),
    c(0.333, 0.333, 0.333, 0.667, 0.333, 0.333, 0.333, 0.333, 0.333, 0.667)
  )
})


test_that("proportions are correct -- df", {
  
  summary_stats <-
    util_proportions(
      postsynth = df, 
      data = df
    ) 
  
  expect_equal(
    round(summary_stats$prop, 3),
    c(0.333, 0.333, 0.333, 0.667, 0.333, 0.333, 0.333, 0.333, 0.667, 0.333)
  )
})

# full with group_var specified 
test_that("proportions and classes are correct w/ group_var -- postsynth", {
  
  summary_stats <-
    util_proportions(
      postsynth = syn, 
      data = df,
      group_var = c
    ) 
  
  summary_stats <- summary_stats |>
    arrange(source)
  
  expect_equal(
    round(summary_stats$prop, 3),
    c(0.5, 0.5, 1, 1, 0.5, 0.5)
  )
  
})

test_that("proportions and classes are correct w/ group_var -- df", {
  
  summary_stats <-
    util_proportions(
      postsynth = df, 
      data = df,
      group_var = c
    ) 
  
  summary_stats <- summary_stats |>
    arrange(source)
  
  expect_equal(
    round(summary_stats$prop, 3),
    c(0.5, 0.5, 1, 0.5, 0.5, 1)
  )
  
})