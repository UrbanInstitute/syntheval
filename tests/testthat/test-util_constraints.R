# var1 is for conditions
# var2 range
# var3 allowed
# var4 forbidden

# test df (8 /8 combin ations)
data <- data.frame(
  var1 = c(1, 2, 3, 4),
  var2 = c(NA, 2, 3, 4),
  var3 = c("a", "a", "b", "b"),
  var4 = c(NA, "c", "d", "e")
)

# test synth (4 of 8 combinations)
syn <- list(
  synthetic_data = data,
jth_synthesis_time = data.frame(
    variable = factor(c("var1", "var2", "var3", "var4"))
  )
) |>
  structure(class = "postsynth")

ed <- eval_data(conf_data = data, synth_data = syn, holdout_data = data)

constraints_df_num <- 
  tibble::tribble(
    ~"var", ~"min", ~"max", ~"conditions",
    "var2", 0, 3, "var1 <= 2"
  )

constraints_df_cat <- tibble::tribble(
  # required column names
  ~var, ~allowed, ~forbidden, ~conditions, 
  "var3", "a", NA, "TRUE",
  "var3", "b", NA, "var4 %in% c('c', 'd', 'e')",
  "var4", NA, "e", "var1 >= 3" 
)

test_that("categorical constraints are correct", {

  result <- util_constraints(
    eval_data = ed, 
    constraints_df_num = constraints_df_num,
    constraints_df_cat = constraints_df_cat
  )
  
  target <- tibble::tibble(
    source = rep(c("conf_data", "synth_data", "holdout_data"), each = 3),
    var = rep(c("var3", "var3", "var4"), times = 3),
    allowed = rep(c("a", "b", NA), times = 3),
    forbidden = rep(c(NA, NA, "e"), times = 3),
    conditions = rep(c("TRUE", "var4 %in% c('c', 'd', 'e')", "var1 >= 3"), times = 3),
    n_constraints_applies = rep(c(4, 3, 2), times = 3),
    n_constraints_met = rep(c(2, 2, 1), times = 3),
    prop_constraint_applies = rep(c(1, 0.75, 0.5), times = 3),
    prop_constraints_met = rep(c(0.5, 2 / 3, 0.5), times = 3)
  )
  
  expect_equal(result[["constraints_cat"]], target)
  
  
})



test_that("numeric constraints are correct", {
  
  result <- util_constraints(
    eval_data = ed, 
    constraints_df_num = constraints_df_num
  )
  
  target <- tibble::tibble(
    source = c("conf_data", "synth_data", "holdout_data"),
    var = "var2",
    min = 0,
    max = 3,
    conditions = "var1 <= 2",
    n_constraints_applies = 2,
    n_constraints_met = as.numeric(NA),
    prop_constraint_applies = 0.5,
    prop_constraints_met = as.numeric(NA)
  )
  
  expect_equal(result[["constraints_num"]], target)
  
})

test_that("numeric constraints are correct with na.rm = TRUE", {
  
  result <- util_constraints(
    eval_data = ed, 
    constraints_df_num = constraints_df_num,
    na.rm = TRUE
  )
  
  target <- tibble::tibble(
    source = c("conf_data", "synth_data", "holdout_data"),
    var = "var2",
    min = 0,
    max = 3,
    conditions = "var1 <= 2",
    n_constraints_applies = 2,
    n_constraints_met = 1,
    prop_constraint_applies = 0.5,
    prop_constraints_met = 0.5
  )
  
  expect_equal(result[["constraints_num"]], target)
  
})

test_that("util_constraints() errors with multiple synthetic data sets", {
  
  ed_multi <- eval_data(conf_data = data, synth_data = list(data, data))
  
  expect_error(
    util_constraints(eval_data = ed_multi, constraints_df_num = constraints_df_num),
    regexp = "only one synthesis"
  )
  
})

test_that("util_constraints() errors on wrong constraint column names", {
  
  bad_num <- dplyr::rename(constraints_df_num, minimum = min)
  
  bad_cat <- dplyr::rename(constraints_df_cat, allow = allowed)
  
  expect_error(
    util_constraints(eval_data = ed, constraints_df_num = bad_num),
    regexp = "must have columns"
  )
  
  expect_error(
    util_constraints(eval_data = ed, constraints_df_cat = bad_cat),
    regexp = "must have columns"
  )
  
})

test_that("util_constraints() errors when allowed and forbidden are both set or both NA", {
  
  both <- tibble::tribble(
    ~var, ~allowed, ~forbidden, ~conditions,
    "var3", "a", "b", "TRUE"
  )
  
  neither <- tibble::tribble(
    ~var, ~allowed, ~forbidden, ~conditions,
    "var3", NA, NA, "TRUE"
  )
  
  expect_error(
    util_constraints(eval_data = ed, constraints_df_cat = both),
    regexp = "exactly one"
  )
  
  expect_error(
    util_constraints(eval_data = ed, constraints_df_cat = neither),
    regexp = "exactly one"
  )
  
})
test_that("missing categorical values are not counted as met", {
  
  na_data <- data.frame(var4 = c(NA, NA, "e"))
  
  ed_na <- eval_data(conf_data = na_data, synth_data = na_data)
  
  forbidden <- tibble::tribble(
    ~var, ~allowed, ~forbidden, ~conditions,
    "var4", NA, "e", "TRUE"
  )
  
  result <- util_constraints(eval_data = ed_na, constraints_df_cat = forbidden)
  
  expect_true(all(is.na(result[["constraints_cat"]][["n_constraints_met"]])))
  
  result_rm <- util_constraints(eval_data = ed_na, constraints_df_cat = forbidden, na.rm = TRUE)
  
  expect_equal(result_rm[["constraints_cat"]][["n_constraints_met"]], c(0, 0))
  
})