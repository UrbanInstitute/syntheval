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
  a = c(1000, 1000, NA),
  b = c(1200, 800, NA),
  c = c("1", "1", "2"),
  d = c(0, 0, 10), 
  e = c("a", "b", "b"),
  weight = c(100, 100, 200)
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
    b = c(1000, 1000, NA),
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

ed0 <- eval_data(synth_data = df, conf_data = df)
ed1 <- eval_data(synth_data = syn, conf_data = df)

# full unweighted - postysynth
test_that("moments full unweighted -- postsynth ", {
  
  summary_stats <- util_moments(ed1) %>%
    dplyr::filter(variable == "a")
  
  expect_equal(
    summary_stats$original,
    c(3, 1000, 0, NaN, NaN)
  )
  expect_equal(
    round(summary_stats$synthetic, 2),
    c(2, 800, 721.11, -0.47, -1.5)
  )
})

test_that("moments full unweighted -- df ", {
  
  summary_stats <- util_moments(ed0) %>%
    dplyr::filter(variable == "a")
  
  expect_equal(
    summary_stats$original,
    c(3, 1000, 0, NaN, NaN)
  )
  expect_equal(
    round(summary_stats$synthetic, 2),
    c(3, 1000, 0, NaN, NaN)
  )
})

# full weighted
test_that("moments full weighted -- postsynth", {
  
  summary_stats <- util_moments(ed1, weight_var = weight) %>%
    dplyr::filter(variable == "a")
  
  expect_equal(
    summary_stats$original,
    c(400, 1000, 0, NaN, NaN)
  )
  expect_equal(
    round(summary_stats$synthetic, 2),
    c(250, 775, 765.32, -0.34, -1.69)
  )
})

test_that("moments full weighted -- df", {
  
  summary_stats <- util_moments(ed0, weight_var = weight) %>%
    dplyr::filter(variable == "a")
  
  expect_equal(
    summary_stats$original,
    c(400, 1000, 0, NaN, NaN)
  )
  expect_equal(
    round(summary_stats$synthetic, 2),
    c(400, 1000, 0, NaN, NaN)
  )
})

test_that("moments nonzero unweighted -- postsynth", {
  
  summary_stats <- util_moments(ed1, drop_zeros = TRUE) %>%
    dplyr::filter(variable == "d")
  
  expect_equal(
    summary_stats$original,
    c(1, 10, NaN, NaN, NaN)
  )
  expect_equal(
    round(summary_stats$synthetic, 2),
    c(2, 15, 7.07, 0, -2)
  )
})

test_that("moments nonzero unweighted -- postsynth", {
  
  summary_stats <- util_moments(ed0, drop_zeros = TRUE) %>%
    dplyr::filter(variable == "d")
  
  expect_equal(
    summary_stats$original,
    c(1, 10, NaN, NaN, NaN)
  )
  expect_equal(
    round(summary_stats$synthetic, 2),
    c(1, 10, NaN, NaN, NaN)
  )
})

test_that("moments nonzero weighted -- postsynth", {
  
  summary_stats <- util_moments(
      ed1,
      weight_var = weight,
      drop_zeros = TRUE
    ) %>%
    dplyr::filter(variable == "d")
  
  expect_equal(
    summary_stats$original,
    c(200, 10, NaN, NaN, NaN)
  )
  expect_equal(
    round(summary_stats$synthetic, 2),
    c(300, 15, 7.07, 0, -2)
  )
})

test_that("moments nonzero weighted -- df", {
  
  summary_stats <- util_moments(
      ed0,
      weight_var = weight,
      drop_zeros = TRUE
    ) %>%
    dplyr::filter(variable == "d")
  
  expect_equal(
    summary_stats$original,
    c(200, 10, NaN, NaN, NaN)
  )
  expect_equal(
    round(summary_stats$synthetic, 2),
    c(200, 10, NaN, NaN, NaN)
  )
})

test_that("moments test grouping var ", {

  summary_stats <- util_moments(
      ed1,
      weight_var = weight,
      group_by = c
    ) %>%
    dplyr::filter(variable == "d", statistic %in% c("count", "mean"))
  
  expect_equal(
    summary_stats$original,
    c(0, 200, 0, 10)
  )
  expect_equal(
    round(summary_stats$synthetic, 2),
    c(300, 0, 15, 0)
  )


})

test_that("moments grouping by multiple variables", {
  summary_stats <- util_moments(
      ed1,
      weight_var = weight,
      group_by = c(c, e)
    ) %>%
    dplyr::filter(variable == "d", statistic %in% c("count", "mean"))
  
  expect_equal(
    summary_stats$original,
    c(0, 0, 200, 0, 0, 10)
  )
  expect_equal(
    round(summary_stats$synthetic, 2),
    c(150, 150, 0, 20, 10, 0)
  )
  
})

test_that("util_moments() variables selection returns correct dimensions ", {

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
  expect_message(
    expect_false(
      util_moments(
        ed, 
        common_vars = FALSE
      )$variable %>%
        is.na() %>%
        all()
    )
  )


  # are statistics names missing ever?
  expect_message(
    expect_false(
      util_moments(
        ed, 
        common_vars = FALSE
      )$statistic %>%
        is.na() %>%
        all()
    )
  )
  
  # 55 rows = all 11 variables times 5 statistics
  expect_message(
    expect_equal(
      dim(
        util_moments(
          ed,
          common_vars = FALSE
        )
      ),
      c(55, 6)
    )
  )

  # 50 rows = 10 common variables times 5 statistics
  expect_message(
    expect_equal(
      dim(
        util_moments(
          ed, 
          common_vars = TRUE
        )
      ),
      c(50, 6)
    )
  )

 
  # 15 rows = 3 synthesized numeric variables times 5 statistics
  expect_equal(
    dim(
      util_moments(
        ed, 
        common_vars = FALSE, 
        synth_vars = c("month", "day", "year")
      )
    ),
    c(15, 6)
  )
  
  # 15 rows = 3 synthesized numeric variables times 5 statistics
  expect_equal(
    dim(
      util_moments(
        ed,
        common_vars = TRUE, 
        synth_vars = c("month", "day", "year")
      )
    ),
    c(15, 6)
  )
  
})


test_that("util_moments na.rm works as expected", {
  
  ed <- eval_data(synth_data = syn_na, conf_data = df_na)
  
  expect_message(
    res <- util_moments(
      ed,
      na.rm = FALSE
    )
  )
  expect_true(
    all(is.na(res[res["variable"] == "a", "original"]))
  )
  
  res_rm <- util_moments(
    ed,
    na.rm = TRUE
  )

  expect_true(
    all(res_rm[2, c("original", "synthetic")] == c(1000, 800))
  )
})