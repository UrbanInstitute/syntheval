df_with_nas <- data.frame(
  a = c(1000, 1000, 1000, NA),
  b = c(1200, 800, 1000, NA),
  c = c("1", "1", "2", NA),
  d = c(0, 0, 10, 0), 
  e = c("a", "b", "b", "b"),
  weight = c(100, 100, 200, 100)
)

test_that("prep_combined_data_for_na.rm functionality", {
  
  # if NAs present, raise warning
  expect_message(
    prep_result <- prep_combined_data_for_na.rm(df_with_nas),
    "Some variables contain missing data:  a, b, c"
  )
  
  # passing no optional arguments returns same dataframe
  expect_identical(
    df_with_nas, 
    prep_result)
  
  # passing a dataframe with NAs with drop_zeros raises an error 
  expect_error(
    expect_message(
      prep_combined_data_for_na.rm(df_with_nas, drop_zeros = TRUE)
    )
  )
  
  expect_no_error(
    prep_combined_data_for_na.rm(
      df_with_nas, 
      na.rm = TRUE, 
      drop_zeros = TRUE
    )
  )
  
  # drop_zeros works as expected
  expect_identical(
    prep_combined_data_for_na.rm(
      df_with_nas[, c("weight", "d")],
      drop_zeros = TRUE
      )[, "d"], 
    c(NA, NA, 10, NA)
  )
  
  # drop_zeros optionally ignores unspecified columns
  expect_identical(
    prep_combined_data_for_na.rm(
      df_with_nas[, c("weight", "d")],
      drop_zeros = TRUE,
      drop_zeros_exclude = rlang::sym("d")
    )[, "d"], 
    c(0, 0, 10, 0)
  )
  
})

df_with_sentinel <- data.frame(
  a = c(1, -99, 3, 4),
  b = c("x", "y", "-99", "z")
)

test_that(".recode_custom_na functionality", {
  
  # na_values = NULL returns data unchanged
  expect_identical(
    syntheval:::.recode_custom_na(df_with_sentinel, na_values = NULL),
    df_with_sentinel
  )
  
  # a custom sentinel is recoded to NA across all columns
  recoded <- syntheval:::.recode_custom_na(df_with_sentinel, na_values = -99)
  
  expect_identical(recoded$a, c(1, NA, 3, 4))
  expect_identical(recoded$b, c("x", "y", NA, "z"))
  
  # a vector of sentinels is recoded to NA across all columns
  recoded_vec <- syntheval:::.recode_custom_na(df_with_sentinel, na_values = c(-99, "z"))
  
  expect_identical(recoded_vec$a, c(1, NA, 3, 4))
  expect_identical(recoded_vec$b, c("x", "y", NA, NA))
  
})



