# original data
df <- data.frame(
  a = c(1, 2, 1),
  b = c("orange", "yellow", "green"),
  c = as.factor(c("1", "1", "2")),
  weight = c(100, 100, 200)
)

df_na <- data.frame(
  a = c(1, 2, 1, 1),
  b = c("orange", "yellow", "green", NA),
  c = factor(c("1", "1", "2", NA), 
             levels = c("1", "2", "3", NA),
             exclude = NULL),
  weight = c(100, 100, 200, 100)
)

# postsynth data object
syn <- list(
  synthetic_data = data.frame(
    a = c(2, 2, 2),
    b = c("orange", "yellow", "green"),
    c = as.factor(c("1", "2", "2")),
    weight = c(150, 150, 100)
  ),
  jth_synthesis_time = data.frame(
    variable = factor(c("b", "c"))
  )
) %>%
  structure(class = "postsynth")

# postsynth data object
syn_na <- list(
  synthetic_data = data.frame(
    a = c(2, 2, 2, NA),
    b = c("orange", "yellow", "green", "green"),
    c = factor(c("1", "1", "2", NA), 
               levels = c("1", "2", "3", NA),
               exclude = NULL),
    weight = c(150, 150, 100, 100)
  ),
  jth_synthesis_time = data.frame(
    variable = factor(c("b", "c"))
  )
) %>%
  structure(class = "postsynth")

ed0 <- eval_data(conf_data = df, synth_data = df) 
ed1 <- eval_data(conf_data = df, synth_data = syn)

# testing variable selection
test_that("testing if proportions only uses fct and chr variables", {
  
  summary_stats <- util_proportions(ed1) 
  
  expect_equal(
    unique(summary_stats$variable),
    c("b", "c")
  )
})

# testing proportions
test_that("testing if proportions are correct -- postsynth", {
  
  summary_stats <- util_proportions(ed1) 
  
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
  
  summary_stats <- util_proportions(ed0) 
  
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
  
  summary_stats <- util_proportions(ed1, group_by = c)
  
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
  
  summary_stats <- util_proportions(ed0, group_by = c)
  
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
  
  summary_stats <- util_proportions(ed1, weight_var = weight)
  
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
  
  summary_stats <- util_proportions(ed0, weight_var = weight)
  
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
      ed1,
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
      ed0,
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
    weight = c(100, 100, 200),
    jth_synthesis_time = data.frame(
      variable = factor(c("var2", "var3", "var4"))
    )
  )
  
  # postsynth data object
  syn2 <- list(
    synthetic_data = data.frame(
      var1 = c(1, 1, 2),
      var2 = c("orange", "orange", "green"),
      var3 = as.factor(c("1", "1", "2")),
      var4 = c("a", "a", "b"),      
      weight = c(50, 150, 200)
    ),
    jth_synthesis_time = data.frame(
      variable = factor(c("var2", "var3", "var4"))
    )
  ) %>%
    structure(class = "postsynth")
  
  ed2 <- eval_data(conf_data = df2, synth_data = syn2)
  
  summary_stats <-
    util_proportions(
      ed2,
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

test_that("util_proportions() variables selection returns correct dimensions ", {
  
  storms_sub <- dplyr::select(dplyr::storms, -name, -pressure)
  
  set.seed(1)
  syn <- list(
    synthetic_data = dplyr::slice_sample(
      dplyr::storms,
      n = 1000
    ),
    jth_synthesis_time = data.frame(
      variable = factor("status")
    )
  ) %>%
    structure(class = "postsynth")
  
  ed <- eval_data(conf_data = storms_sub, synth_data = syn)
  
  # are variable names missing ever?
  expect_false(
    util_proportions(
      ed, 
      common_vars = FALSE, 
      synth_vars = FALSE
    )$variable %>%
      is.na() %>%
      all()
  )
  
  # are statistics names missing ever?
  expect_false(
    util_proportions(
      ed,
      common_vars = FALSE, 
      synth_vars = FALSE
    )$class %>%
      is.na() %>%
      all()
  )
  
  # 55 rows = all 11 variables times 5 statistics
  expect_equal(
    dim(
      util_proportions(
        ed,
        common_vars = FALSE, 
        synth_vars = FALSE
      )
    ),
    c(215, 5)
  )
  
  # 50 rows = 10 common variables times 5 statistics
  expect_equal(
    dim(
      util_proportions(
        ed,
        common_vars = TRUE, 
        synth_vars = FALSE
      )
    ),
    c(9, 5)
  )
  
  # 9 rows = 9 classes
  expect_equal(
    dim(
      util_proportions(
        ed,
        common_vars = FALSE, 
        synth_vars = TRUE
      )
    ),
    c(9, 5)
  )
  
  # 9 rows = 9 classes
  expect_equal(
    dim(
      util_proportions(
        ed,
        common_vars = TRUE, 
        synth_vars = TRUE
      )
    ),
    c(9, 5)
  )
  
})

test_that("na.rm in levels works as expected", {
  
  ed_na <- eval_data(conf_data = df_na, synth_data = syn_na)
  
  res <- util_proportions(ed_na)
  
  expect_identical(
    res$class,
    c("NA", "green", "orange", "yellow", "1", "2", NA)  
  )
  
  res_rm <- util_proportions(ed_na, na.rm = TRUE)
  
  expect_identical(
    res_rm$class,
    c("green", "orange", "yellow", "1", "2")  
  )
  
})

test_that("keep_empty_levels works as expected", {
  
  ed_na <- eval_data(conf_data = df_na, synth_data = syn_na)
  
  res <- util_proportions(
    ed_na,
    keep_empty_levels = TRUE
  )
  
  # expect to show 0 proportion in empty level "3" for variable c
  expect_true(
    all(res[7, c("synthetic", "original")] == c(0, 0))
  )
  
})

