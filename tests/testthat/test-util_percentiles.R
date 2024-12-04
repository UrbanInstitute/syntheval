# test df (8 /8 combinations)
df <- data.frame(
  a = c(1000, 1000, 1000),
  b = c(-1200, -800, 1000),
  c = c("1", "1", "2"),
  d = c(0, 0, 10), 
  e = c("a", "b", "a"),
  weight = c(300, 100, 100)
)

df_na <- data.frame(
  a = c(1000, 1000, 1000, 1000),
  b = c(-1200, -800, 1000, NA),
  c = c("1", "1", "2", "2"),
  d = c(0, 0, 10, 0), 
  e = c("a", "b", "a", "b"),
  weight = c(300, 100, 100, 100)
)

# test synth (4 of 8 combinations)
syn <- list(
  synthetic_data = data.frame(
    a = c(1400, 0, 1000),
    b = c(1000, 1000, 1000),
    c = c("1", "1", "2"),
    d = c(20, 10, 0), 
    e = c("a", "b", "a"),
    weight = c(300, 100, 100)
  ),
  jth_synthesis_time = data.frame(
    variable = factor(c("a", "b", "d", "weight"))
  )
) %>%
  structure(class = "postsynth")

syn_na <- list(
  synthetic_data = data.frame(
    a = c(1400, NA, 1000),
    b = c(1000, 1000, 1000),
    c = c("1", "1", "2"),
    d = c(20, 10, 0), 
    e = c("a", "b", "a"),
    weight = c(300, 100, 100)
  ),
  jth_synthesis_time = data.frame(
    variable = factor(c("a", "b", "d", "weight"))
  )
) %>%
  structure(class = "postsynth")

ed0 <- eval_data(conf_data = df, synth_data = df)
ed1 <- eval_data(conf_data = df, synth_data = syn)
ed_na <- eval_data(conf_data = df_na, synth_data = syn_na)

test_that("unweighted percentiles make sense ", {
  
  test1 <- util_percentiles(ed1, probs = 0.5)
  
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
  
  test2 <- util_percentiles(
    ed1,
    probs = 0.5,
    weight_var = weight,
    synth_vars = FALSE
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

  test3 <- util_percentiles(
    ed1,
    probs = c(0.01, 0.99), 
    weight_var = weight
  )
  
  # does the dimension make sense?
  expect_equal(
    dim(test3),
    c(8, 6)
  )
  
  test4 <- util_percentiles(
    ed1,
    probs = c(0.1, 0.5, 0.9),
    weight_var = weight
  )
  
  # are the correct p labels returned
  expect_equal(
    unique(test4$p),
    c(0.1, 0.5, 0.9)
  )
  
})

test_that("unweighted percentiles grouped by one variable ", {
  
  test5 <- util_percentiles(
    ed1,
    probs = 0.5, 
    group_by = c
  )
  
  # does the dimension make sense?
  expect_equal(
    dim(test5),
    c(8, 7) # additional col for group by variable 
  )
  
  # do the original values make sense
  expect_equal(
    test5$original,
    c(1000, 1000, -1000, 1000, 0, 10, 200, 100)
  )
  
  # do the synthetic values make sense
  expect_equal(
    test5$synthetic,
    c(700, 1000, 1000, 1000, 15, 0, 200, 100)
  )
  
})


test_that("unweighted percentiles grouped by multiple variables ", {
  
  test6 <- util_percentiles(
    ed1,
    probs = 0.5,
    group_by = c(c, e)
  )
  
  # does the dimension make sense?
  expect_equal(
    dim(test6),
    c(12, 8) # additional cols for group by variables 
  )
  
  # do the original values make sense
  expect_equal(
    test6$original,
    c(1000, 1000, 1000, -1200, -800, 1000, 0, 0, 10, 300, 100, 100)
  )
  
  # do the synthetic values make sense
  expect_equal(
    test6$synthetic,
    c(1400, 0, 1000, 1000, 1000, 1000, 20, 10, 0, 300, 100, 100)
  )
  
})








test_that("util_percentiles() variables selection returns correct dimensions ", {
  
  storms_sub <- dplyr::select(dplyr::storms, -name, -pressure) %>%
    dplyr::filter(complete.cases(.))
  
  syn <- list(
    synthetic_data = dplyr::slice_sample(
      dplyr::storms %>%
        dplyr::filter(complete.cases(.)),
      n = 1000
    ),
    jth_synthesis_time = data.frame(
      variable = factor(c("month", "day", "year"))
    )
  ) %>%
    structure(class = "postsynth")
  
  ed <- eval_data(conf_data = storms_sub, synth_data = syn)
  
  # are variable names missing ever?
  expect_false(
    util_percentiles(
      ed,
      common_vars = TRUE, 
      synth_vars = FALSE
    )$variable %>%
      is.na() %>%
      all()
  )
  
  # are statistics names missing ever?
  expect_false(
    util_percentiles(
      ed,
      common_vars = TRUE, 
      synth_vars = FALSE
    )$p %>%
      is.na() %>%
      all()
  )
  
  # error: quantile doesn't work with missing values
  expect_message(
    expect_error(
      util_percentiles(
        ed, 
        common_vars = FALSE, 
        synth_vars = FALSE
      )
    )
  )
  
  # 30 rows = 10 common variables times 3 statistics
  expect_equal(
    dim(
      util_percentiles(
        ed,
        common_vars = TRUE, 
        synth_vars = FALSE
      )
    ),
    c(30, 6)
  )
  
  # 9 rows = 3 synthesized numeric variables times 3 statistics
  expect_equal(
    dim(
      util_percentiles(
        ed,
        common_vars = FALSE, 
        synth_vars = TRUE
      )
    ),
    c(9, 6)
  )
  
  # 9 rows = 3 synthesized numeric variables times 3 statistics
  expect_equal(
    dim(
      util_percentiles(
        ed,
        common_vars = TRUE, 
        synth_vars = TRUE
      )
    ),
    c(9, 6)
  )
  
})

test_that("util_percentiles na.rm", {
  
  # if na.rm = FALSE, raise error for missing values
  expect_message(
    expect_error(
      util_percentiles(
        ed_na,
        probs = 0.5,
        na.rm = FALSE
      )
    )
  )
  
  # else, re
  res <- util_percentiles(
    ed_na,
    probs = 0.5,
    na.rm = TRUE
  )
  
  expect_true(
    all(res[1, c("original", "synthetic")] == c(1000, 1200))
  )
  
})