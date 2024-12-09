# test df (8 /8 combinations)
df <- data.frame(
  a = c(1000, 1000, 1000),
  b = c(1200, 800, 1000),
  c = c("1", "1", "2"),
  d = c(0, 0, 10), 
  e = c("a", "b", "b"),
  weight = c(100, 100, 200)
)

df_na <- data.frame(
  a = c(1000, 1000, 1000, NA),
  b = c(1200, 800, 1000, NA),
  c = c("1", "1", "2", "2"),
  d = c(0, 0, 10, 0), 
  e = c("a", "b", "b", "a"),
  weight = c(100, 100, 200, 100)
)

# test synth (4 of 8 combinations)
syn <- list(
  synthetic_data = data.frame(
    a = c(1400, 0, 1000),
    b = c(1000, 1000, 1000),
    c = c("1", "1", "2"),
    d = c(20, 10, 0), 
    e = c("a", "b", "b"),
    weight = c(150, 150, 100)
  ),
  jth_synthesis_time = data.frame(
    variable = factor(c("a", "b", "d", "weight"))
  )
) %>%
  structure(class = "postsynth")

syn_na <- list(
  synthetic_data = data.frame(
    a = c(1400, 0, 1000),
    b = c(1000, NA, 1000),
    c = c("1", "1", "2"),
    d = c(20, 10, 0), 
    e = c("a", "b", "b"),
    weight = c(150, 150, 100)
  ),
  jth_synthesis_time = data.frame(
    variable = factor(c("a", "b", "d", "weight"))
  )
) %>%
  structure(class = "postsynth")

ed0 <- eval_data(conf_data = df, synth_data = df)
ed1 <- eval_data(conf_data = df, synth_data = syn)

# full unweighted - postysynth
test_that("moments full unweighted -- postsynth ", {
  
  summary_stats <- util_totals(ed1) %>%
    dplyr::filter(variable == "a")
  
  expect_equal(
    summary_stats$original,
    c(3, 3000)
  )
  expect_equal(
    round(summary_stats$synthetic, 2),
    c(2, 2400)
  )
})

test_that("moments full unweighted -- df ", {
  
  summary_stats <- util_totals(ed0) %>%
    dplyr::filter(variable == "a")
  
  expect_equal(
    summary_stats$original,
    c(3, 3000)
  )
  expect_equal(
    round(summary_stats$synthetic, 2),
    c(3, 3000)
  )
})

# full weighted
test_that("moments full weighted -- postsynth", {
  
  summary_stats <- util_totals(ed1, weight_var = weight) %>%
    dplyr::filter(variable == "a")
  
  expect_equal(
    summary_stats$original,
    c(400, 400000)
  )
  expect_equal(
    round(summary_stats$synthetic, 2),
    c(250, 310000)
  )
})

test_that("moments full weighted -- df", {
  
  summary_stats <- util_totals(ed0, weight_var = weight) %>%
    dplyr::filter(variable == "a")
  
  expect_equal(
    summary_stats$original,
    c(400, 400000)
  )
  expect_equal(
    round(summary_stats$synthetic, 2),
    c(400, 400000)
  )
})

test_that("moments nonzero unweighted -- postsynth", {
  
  summary_stats <- util_totals(ed1) %>%
    dplyr::filter(variable == "d")
  
  expect_equal(
    summary_stats$original,
    c(1, 10)
  )
  expect_equal(
    round(summary_stats$synthetic, 2),
    c(2, 30)
  )
})

test_that("moments nonzero unweighted -- postsynth", {
  
  summary_stats <- util_totals(ed0) %>%
    dplyr::filter(variable == "d")
  
  expect_equal(
    summary_stats$original,
    c(1, 10)
  )
  expect_equal(
    round(summary_stats$synthetic, 2),
    c(1, 10)
  )
})

test_that("moments nonzero weighted -- postsynth", {
  
  summary_stats <- util_totals(ed1, weight_var = weight) %>%
    dplyr::filter(variable == "d")
  
  expect_equal(
    summary_stats$original,
    c(200, 2000)
  )
  expect_equal(
    round(summary_stats$synthetic, 2),
    c(300, 4500)
  )
})

test_that("moments nonzero weighted -- df", {
  
  summary_stats <- util_totals(ed0, weight_var = weight) %>%
    dplyr::filter(variable == "d")
  
  expect_equal(
    summary_stats$original,
    c(200, 2000)
  )
  expect_equal(
    round(summary_stats$synthetic, 2),
    c(200, 2000)
  )
})

test_that("moments test grouping var ", {

  summary_stats <- util_totals(
      ed1,
      weight_var = weight,
      group_by = c
    ) %>%
    dplyr::filter(variable == "d")
  
  expect_equal(
    summary_stats$original,
    c(0, 200, 0, 2000)
  )
  expect_equal(
    round(summary_stats$synthetic, 2),
    c(300, 0, 4500, 0)
  )


})

test_that("moments grouping by multiple variables", {
  summary_stats <- 
    util_totals(
      ed1,
      weight_var = weight,
      group_by = c(c, e)
    ) %>%
    dplyr::filter(variable == "d")
  
  expect_equal(
    summary_stats$original,
    c(0, 0, 200, 0, 0, 2000)
  )
  expect_equal(
    round(summary_stats$synthetic, 2),
    c(150, 150, 0, 3000, 1500, 0)
  )
  
})





test_that("util_totals() variables selection returns correct dimensions ", {
  
  storms_sub <- dplyr::select(dplyr::storms, -name, -pressure)
  
  syn <- list(
    synthetic_data = dplyr::slice_sample(
      dplyr::storms,
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
    util_totals(
      ed, 
      common_vars = FALSE, 
      synth_vars = FALSE
    )$variable %>%
      is.na() %>%
      all()
  )
  
  # are statistics names missing ever?
  expect_false(
    util_totals(
      ed, 
      common_vars = FALSE, 
      synth_vars = FALSE
    )$statistic %>%
      is.na() %>%
      all()
  )
  
  # 22 rows = all 11 variables times 2 statistics
  expect_equal(
    dim(
      util_totals(
        ed, 
        common_vars = FALSE, 
        synth_vars = FALSE
      )
    ),
    c(22, 6)
  )
  
  # 20 rows = 10 common variables times 2 statistics
  expect_equal(
    dim(
      util_totals(
        ed, 
        common_vars = TRUE, 
        synth_vars = FALSE
      )
    ),
    c(20, 6)
  )
  
  # 6 rows = 3 synthesized numeric variables times 2 statistics
  expect_equal(
    dim(
      util_totals(
        ed, 
        common_vars = FALSE, 
        synth_vars = TRUE
      )
    ),
    c(6, 6)
  )
  
  # 6 rows = 3 synthesized numeric variables times 2 statistics
  expect_equal(
    dim(
      util_totals(
        ed, 
        common_vars = TRUE, 
        synth_vars = TRUE
      )
    ),
    c(6, 6)
  )
  
})

test_that("util_totals() na.rm works as expected", {
  
  ed_na <- eval_data(conf_data = df_na, synth_data = syn_na)
  
  res <- util_totals(
    ed_na,
    na.rm = FALSE
  )
  
  expect_true(
    all(is.na(res[1:4, "original"]))
  )
  
  res_rm <- util_totals(
    ed_na,
    na.rm = TRUE
  )
  
  expect_true(
    all(res_rm[2, c("original", "synthetic")] == c(3000, 2400))
  )
  
})
