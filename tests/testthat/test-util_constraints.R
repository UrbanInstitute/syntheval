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

test_that("categorical constraints are correct ", {

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



test_that("numeric constraints are correct ", {
  
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

test_that("numeric constraints are correct with na.rm = TRUE ", {
  
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
